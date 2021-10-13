(require 'org-glance-module)

(org-glance:require
  org
  org-element
  lib.core.exceptions
  lib.utils.helpers
  lib.utils.org)

(cl-defun org-glance-headline-p (&optional (headline (org-element-at-point)))
  "Assume HEADLINE is an `org-element' with :ORG_GLANCE_ID property specified.
Return headline or nil if it is not a proper `org-glance-headline'."
  (when (org-element-property :ORG_GLANCE_ID headline)
    headline))

(defvar org-glance-headline:serde-alist
  `((:raw-value . org-glance-headline:title)
    (:begin . org-glance-headline:begin)
    (:file . org-glance-headline:file)
    (:commentedp . org-glance-headline:commented?)
    (:archivedp . org-glance-headline:archived?)
    (:linkedp . org-glance-headline:linked?)
    (:kvp . org-glance-headline:kvp?)
    (:encryptedp . org-glance-headline:encrypted?))
  "Map `org-element-property' to `org-glance' extractor method.

It is safe (in terms of backward/forward compability of
metastores) to append properties to this map.

Do not modify existing properties without backfilling of
metastore.")

(cl-defun org-glance-headline:serialize (headline)
  "Serialize HEADLINE to store it on disk."
  (cl-loop
     for (property . method) in org-glance-headline:serde-alist
     collect (funcall method headline)))

(cl-defun org-glance-headline:deserialize (dump)
  "Deserialize DUMP to minimal headline."
  (cl-loop
     with element = (org-element-create 'headline)
     for (property . method) in org-glance-headline:serde-alist
     for index from 0
     do (org-glance-headline:enrich element property (nth index dump))
     finally (return element)))

(cl-defun org-glance-headline:enrich (element &rest kwargs)
  "Enrich `org-element' ELEMENT with KWARGS properties."
  (declare (indent 1))
  (cl-loop
     for (key value) on kwargs by #'cddr
     do (org-element-put-property element key value)
     finally (return element)))

(cl-defun org-glance-headline:create (&optional (prototype (org-element-at-point)))
  (when (eql 'headline (org-element-type prototype))
    (cl-loop
       for (property . method) in org-glance-headline:serde-alist
       for index from 0
       do (org-glance-headline:enrich prototype property (funcall method prototype))
       finally (return prototype))))

(cl-defun org-glance-headline:search-parents ()
  "Traverse parents in search of a proper `org-glance-headline'."
  (org-glance:ensure-at-heading)
  (beginning-of-line)
  (while (and (not (org-glance-headline-p))
              (> (point) (point-min)))
    (org-up-heading-or-point-min))
  (org-glance-headline:enrich (org-glance-headline:create)
    :encryptedp (save-excursion
                  (org-end-of-meta-data t)
                  (looking-at "aes-encrypted V [0-9]+.[0-9]+-.+\n"))))

(cl-defun org-glance-headline:at-point ()
  "Search for the first occurence of `org-glance-headline' in parent headlines."
  (save-excursion
    (org-glance-headline:search-parents)))

(cl-defun org-glance-headline:active? (&optional (headline (org-element-at-point)))
  (and (org-glance-headline-p headline)
       (not (org-glance-headline:commented? headline))
       (not (org-glance-headline:archived? headline))))

(cl-defun org-glance-headline:search-backward ()
  (interactive)
  (while (and (outline-previous-heading) (not (org-glance-headline:active?))))
  (when (org-glance-headline:active?) (org-glance-headline:at-point)))

(cl-defun org-glance-headline:search-forward ()
  (interactive)
  (while (and (outline-next-heading) (not (org-glance-headline:active?))))
  (when (org-glance-headline:active?) (org-glance-headline:at-point)))

(cl-defun org-glance-headline:id (&optional (headline (org-glance-headline:at-point)))
  "Return unique identifer of HEADLINE."
  (org-element-property :ORG_GLANCE_ID headline))

(cl-defun org-glance-headline:state (&optional (headline (org-glance-headline:at-point)))
  (substring-no-properties (or (org-element-property :todo-keyword headline) "")))

(cl-defun org-glance-headline:commented? (&optional (headline (org-glance-headline:at-point)))
  (when (org-element-property :commentedp headline)
    t))

(cl-defun org-glance-headline:archived? (&optional (headline (org-glance-headline:at-point)))
  (when (org-element-property :archivedp headline)
    t))

(cl-defun org-glance-headline:raw-value (&optional (headline (org-glance-headline:at-point)))
  (org-element-property :raw-value headline))

(cl-defun org-glance-headline:title (&optional (headline (org-glance-headline:at-point)))
  (or (org-element-property :TITLE headline)
      (org-element-property :raw-value headline)
      ""))

(cl-defun org-glance-headline:priority (&optional (headline (org-glance-headline:at-point)))
  (org-element-property :priority headline))

(cl-defun org-glance-headline:creation-time (&optional (headline (org-glance-headline:at-point)))
  (org-element-property :ORG_GLANCE_CREATION_TIME headline))

(cl-defun org-glance-headline:modtime (&optional (headline (org-glance-headline:at-point)))
  (-some-> headline
    org-glance-headline:file
    file-attributes
    file-attribute-modification-time
    (format-time-string "%Y-%m-%d %H:%M:%S")))

(cl-defun org-glance-headline:file (&optional (headline (org-glance-headline:at-point)))
  (when-let (file (if (plist-member (nth 1 headline) :file)
                      (org-element-property :file headline)
                    (buffer-file-name)))
    (abbreviate-file-name file)))

(cl-defun org-glance-headline:level (&optional (headline (org-glance-headline:at-point)))
  (org-element-property :level headline))

(cl-defun org-glance-headline:buffer (&optional (headline (org-glance-headline:at-point)))
  (or (org-element-property :buffer headline)
      (condition-case nil
          (get-file-buffer (org-glance-headline:file headline))
        (wrong-type-argument (current-buffer)))))

(cl-defun org-glance-headline:begin (&optional (headline (org-glance-headline:at-point)))
  (org-element-property :begin headline))

(cl-defun org-glance-headline:end (&optional (headline (org-glance-headline:at-point)))
  (org-glance-headline:narrow headline
    (save-excursion (org-end-of-subtree t))))

(cl-defun org-glance-headline:class (&optional (headline (org-glance-headline:at-point)))
  (org-element-property :ORG_GLANCE_CLASS headline))

(cl-defun org-glance-headline:tags (&optional (headline (org-glance-headline:at-point)))
  (mapcar #'s-titleized-words (org-element-property :tags headline)))

(cl-defun org-glance-headline:search-buffer-by-id (id)
  (let ((points (org-element-map (org-element-parse-buffer 'headline) 'headline
                  (lambda (elem) (when (string= (org-glance-headline:id elem) id)
                              (org-element-property :begin elem))))))
    (unless points
      (org-glance-exception:HEADLINE-NOT-FOUND "Headline not found in file %s: %s" (buffer-file-name) id))
    (when (> (length points) 1)
      (org-glance:log-warning "Headline ID %s is not unique in file %s" id (buffer-file-name)))
    (goto-char (car points))
    (org-glance-headline:at-point)))

(cl-defun org-glance-headline:visit (&optional (headline (org-glance-headline:at-point)))
  "Visit HEADLINE."
  (let* ((id (org-glance-headline:id headline))
         (file (org-glance-headline:file headline))
         (buffer (org-glance-headline:buffer headline))
         (revert-without-query (list file)))

    (cond ((and file (file-exists-p file)) (find-file file))
          ((and buffer (buffer-live-p buffer)) (switch-to-buffer buffer))
          (t (org-glance:log-warning "File and buffer not found for visiting. Using current buffer...")))

    (org-glance:log-debug "We are now visiting headline buffer %s, let's remove restrictions" (current-buffer))
    (widen)
    (org-glance:log-debug "Search headline in buffer")
    (cond (id (org-glance-headline:search-buffer-by-id id))
          (t (goto-char (org-glance-headline:begin headline))))))

(cl-defmacro org-glance-headline:narrow (headline &rest forms)
  "Visit HEADLINE, narrow to its subtree and execute FORMS on it."
  (declare (indent 1) (debug t))
  `(save-window-excursion
     (save-excursion
       (save-restriction
         (org-glance-headline:visit ,headline)
         (org-narrow-to-subtree)
         ,@forms))))

(cl-defun org-glance-headline:promote-to-the-first-level ()
  (org-glance:ensure-at-heading)
  (while (and (org-glance-headline-p) (looking-at "^\\*\\*"))
    (org-promote-subtree)))

(cl-defun org-glance-headline:contents (&optional (headline (org-glance-headline:at-point)))
  (let ((file (org-glance-headline:file headline))
        (buffer (org-glance-headline:buffer headline)))
    (cond (file (with-temp-buffer
                  (org-glance:log-debug "Extract contents for headline %s from file %s" (org-glance-headline:id headline) file)
                  (org-mode)
                  (insert-file-contents file)
                  (org-glance-headline:search-buffer-by-id (org-glance-headline:id headline))
                  (org-narrow-to-subtree)
                  (goto-char (point-min))
                  (org-glance-headline:promote-to-the-first-level)
                  (s-trim (buffer-substring-no-properties (point-min) (point-max)))))
          (buffer (with-current-buffer buffer
                    (org-glance:log-debug "Extract contents for headline %s from buffer %s" (org-glance-headline:id headline) file)
                    (save-window-excursion
                      (save-excursion
                        (save-restriction
                          (widen)
                          (org-glance-headline:search-buffer-by-id (org-glance-headline:id headline))
                          (org-narrow-to-subtree)
                          (org-glance-headline:promote-to-the-first-level)
                          (s-trim (buffer-substring-no-properties (point-min) (point-max))))))))
          (t (org-glance-exception:HEADLINE-NOT-FOUND "Unable to determine headline location.")))))

(cl-defgeneric org-glance-headline:extract (scope)
  "Extract `org-glance-headlines' from scope.")

(cl-defmethod org-glance-headline:extract ((f string))
  "Extract headlines from file F."
  (if-let (b (get-buffer f)) ;; buffer name
      (org-glance-headline:extract b)
    (with-temp-buffer
      (org-glance:log-debug "Scan file %s" f)
      (insert-file-contents f)
      (org-mode)
      (cl-loop
         for headline in (org-glance-headline:extract (current-buffer))
         collect (org-glance-headline:enrich headline :file (abbreviate-file-name f))))))

(cl-defmethod org-glance-headline:extract ((b buffer))
  "Extract headlines from buffer B."
  (with-current-buffer b
    (org-element-map (org-element-parse-buffer 'headline) 'headline
      (lambda (e)
        (when (org-glance-headline-p e)
          (org-glance-headline:enrich (org-glance-headline:create e)
            :buffer b))))))

(cl-defun org-glance-headline:add-log-note (note &optional (headline (org-glance-headline:at-point)))
  (org-glance-headline:narrow headline
    (goto-char (org-log-beginning t))
    (insert note "\n")
    (when (buffer-file-name)
      (save-buffer))))

(cl-defun org-glance-headline:rename (headline title)
  (org-glance-headline:narrow headline
    (let ((old-title (org-glance-headline:raw-value headline))
          (new-title (replace-regexp-in-string "[[:space:]]" " " title)))
      (org-glance-headline:add-log-note
       (org-glance:format "- Renamed from \"${old-title}\" to \"${new-title}\" on ${now}"))
      (org-glance-headline:search-parents)
      (org-beginning-of-line)
      (org-kill-line)
      (insert new-title)
      (org-align-tags))))

(cl-defmacro org-glance-headline:format (headline &key (format "${label} [[org-glance-visit:${id}][${stateful-title}]]"))
  (declare (indent 1) (debug t))
  `(let* ((id (org-glance-headline:id ,headline))
          (state (org-glance-headline:state ,headline))
          (label (if (string-empty-p state) " " (format " *%s* " state)))
          (original-title (org-glance-headline:title ,headline))
          (stateful-title (replace-regexp-in-string (format "^%s[[:space:]]*" state) "" original-title))
          (now (format-time-string (org-time-stamp-format 'long 'inactive) (current-time))))
     (s-trim (org-glance:format ,format))))

(cl-defun org-glance-headline:encrypt (&optional password)
  "Encrypt subtree at point with PASSWORD."
  (interactive)
  (let ((beg (save-excursion (org-end-of-meta-data t) (point)))
        (end (save-excursion (org-end-of-subtree t) (point))))
    (org-glance:encrypt-region beg end password)))

(cl-defun org-glance-headline:decrypt (&optional password)
  "Decrypt subtree at point with PASSWORD."
  (interactive)
  (let ((beg (save-excursion (org-end-of-meta-data t) (point)))
        (end (save-excursion (org-end-of-subtree t) (point))))
    (org-glance:decrypt-region beg end password)))

(cl-defun org-glance-headline:demote (level)
  (cl-loop repeat level
     do (org-with-limited-levels
         (org-map-tree 'org-demote))))

;; Relations

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

(cl-defun org-glance-headline:hash (&optional (headline (org-glance-headline:at-point)))
  (let ((contents (org-glance-headline:contents headline)))
    (with-temp-buffer
      (insert contents)
      (buffer-hash))))

(cl-defun org-glance-headline:add-relation
    (source target &key (rel org-glance-relation:forward))
  (interactive)
  (let* ((target-title (org-glance-headline:format target))
         (now (format-time-string (org-time-stamp-format 'long 'inactive) (current-time)))
         (log-entry (org-glance:format "- ${rel} ${target-title} on ${now}")))
    (org-glance-headline:add-log-note log-entry source)))

(cl-defun org-glance-headline:add-biconnected-relation
    (source target
     &key
       (source->target org-glance-relation:forward)
       (target->source org-glance-relation:backward))
  (when source
    (save-excursion
      (save-restriction
        (org-save-outline-visibility t
          (org-glance-headline:add-relation source target :rel source->target)
          (unless (eql source target)
            (org-glance-headline:add-relation target source :rel target->source)))))))

(cl-defun org-glance-headline:kvp? (&optional (headline (org-glance-headline:at-point)))
  ;; (if (plist-member (nth 1 headline) :kvp)
  ;;     (org-element-property :kvp headline)
  ;;   (org-glance-headline:narrow headline
  ;;     (org-end-of-meta-data t)
  ;;     (when (re-search-forward org-glance:key-value-pair-re nil t)
  ;;       t)))
  )

(cl-defun org-glance-headline:linked? (&optional (headline (org-glance-headline:at-point)))
  ;; (if (plist-member (nth 1 headline) :linkedp)
  ;;     (org-element-property :linkedp headline)
  ;;   (org-glance-headline:narrow headline
  ;;     (org-end-of-meta-data t)
  ;;     (when (re-search-forward org-any-link-re nil t)
  ;;       t)))
  )

(cl-defun org-glance-headline:encrypted? (&optional (headline (org-glance-headline:at-point)))
  (org-element-property :encryptedp headline))

(cl-defun org-glance-headline:classes (&optional (headline (org-glance-headline:at-point)))
  (org-glance-headline:narrow headline
    (mapcar #'intern (org-get-tags))))

(org-glance:provide)
