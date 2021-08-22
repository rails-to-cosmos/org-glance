(require 'org)
(require 'org-element)
(require 'org-glance-module)

(org-glance-module-import lib.core.exceptions)
(org-glance-module-import lib.utils.org)

(cl-defun org-glance-headline-p (&optional (headline (org-element-at-point)))
  "Assume HEADLINE is an `org-element' with :ORG_GLANCE_ID property.
Return HEADLINE or nil if it is not a proper `org-glance-headline'."
  (when (not (null (org-element-property :ORG_GLANCE_ID headline)))
    headline))

(cl-defun org-glance-headline:serialize (headline)
  "Serialize HEADLINE to store it on disk."
  (list (org-glance-headline:title headline)
        (org-glance-headline:begin headline)
        (org-glance-headline:file headline)))

(cl-defun org-glance-headline:deserialize (dump)
  "Deserialize DUMP to minimal headline."
  (cl-destructuring-bind (title begin file) dump
    (org-element-create 'headline
                        (list :raw-value title
                              :begin begin
                              :file file))))

(cl-defun org-glance-headline:goto-beginning-of-current-headline ()
  "Jump to headline at point and build `org-glance-headline' object from `org-element' at point.
If headline is not an `org-glance-headline', traverse parents."
  (org-glance:ensure-at-heading)
  (beginning-of-line)
  (while (and (not (org-glance-headline-p))
              (> (point) (point-min)))
    (org-up-heading-or-point-min))
  (-some-> (org-glance-headline-p)
    (org-glance-headline:enrich :file (buffer-file-name))))

(cl-defun org-glance-headline:at-point ()
  "Search for the first occurence of `org-glance-headline' in parent headlines."
  (save-excursion
    (org-glance-headline:goto-beginning-of-current-headline)))

(cl-defun org-glance-headline:search-backward ()
  (interactive)
  (while (and (outline-previous-heading) (not (org-glance-headline-p))))
  (when (org-glance-headline-p) (org-glance-headline:at-point)))

(cl-defun org-glance-headline:search-forward ()
  (interactive)
  (while (and (outline-next-heading) (not (org-glance-headline-p))))
  (when (org-glance-headline-p) (org-glance-headline:at-point)))

(cl-defun org-glance-headline:enrich (element &rest kwargs)
  "Enrich `org-element' ELEMENT with KWARGS properties.
Default enrichment is as follows:
- Add FILE property to `org-element'."
  (cl-loop
     for (key value) on kwargs by #'cddr
     do (org-element-put-property element key value)
     finally (return element)))

(cl-defun org-glance-headline:fetch (&optional (headline (org-glance-headline:at-point)))
  (let ((id (org-glance-headline:id headline))
        (file (org-glance-headline:file headline)))
    (when (file-exists-p file)
      (with-temp-buffer
        (org-mode)
        (insert-file-contents file)
        (-> (org-glance-headline:search-buffer-by-id id)
          (org-glance-headline:enrich :file file))))))

(cl-defun org-glance-headline:id (&optional (headline (org-glance-headline:at-point)))
  "Return unique identifer of HEADLINE."
  (org-element-property :ORG_GLANCE_ID headline))

(cl-defun org-glance-headline:state (&optional (headline (org-glance-headline:at-point)))
  (substring-no-properties (or (org-element-property :todo-keyword headline) "")))

(cl-defun org-glance-headline:commented? (&optional (headline (org-glance-headline:at-point)))
  (org-element-property :commentedp headline))

(cl-defun org-glance-headline:raw-value (&optional (headline (org-glance-headline:at-point)))
  (org-element-property :raw-value headline))

(cl-defun org-glance-headline:title (&optional (headline (org-glance-headline:at-point)))
  (or (org-element-property :TITLE headline)
      (org-element-property :raw-value headline)))

(cl-defun org-glance-headline:priority (&optional (headline (org-glance-headline:at-point)))
  (org-element-property :priority headline))

(cl-defun org-glance-headline:creation-time (&optional (headline (org-glance-headline:at-point)))
  (org-element-property :ORG_GLANCE_CREATION_TIME headline))

(cl-defun org-glance-headline:modtime (&optional (headline (org-glance-headline:at-point)))
  (-> headline
    org-glance-headline:file
    file-attributes
    file-attribute-modification-time
    (format-time-string "%Y-%m-%d %H:%M:%S")))

(cl-defun org-glance-headline:file (&optional (headline (org-glance-headline:at-point)))
  (org-element-property :file headline))

(cl-defun org-glance-headline:level (&optional (headline (org-glance-headline:at-point)))
  (org-element-property :level headline))

(cl-defun org-glance-headline:buffer (&optional (headline (org-glance-headline:at-point)))
  (get-file-buffer (org-glance-headline:file headline)))

(cl-defun org-glance-headline:begin (&optional (headline (org-glance-headline:at-point)))
  (org-element-property :begin headline))

(cl-defun org-glance-headline:view-ids (&optional (headline (org-glance-headline:at-point)))
  (mapcar #'s-titleized-words (org-element-property :tags headline)))

(cl-defun org-glance-headline:search-buffer-by-id (id)
  (let ((points (org-element-map (org-element-parse-buffer 'headline) 'headline
                  (lambda (elem) (when (string= (org-glance-headline:id elem) id)
                              (org-element-property :begin elem))))))
    (unless points
      (org-glance-exception:headline-not-found "Headline not found in file %s: %s" (buffer-file-name) id))

    (when (> (length points) 1)
      (warn "Headline ID %s is not unique in file %s" id (buffer-file-name)))

    (goto-char (car points))
    (org-glance-headline:at-point)))

(cl-defun org-glance-headline:search-buffer (&optional (headline (org-glance-headline:at-point)))
  (org-glance-headline:search-buffer-by-id (org-glance-headline:id headline)))

(cl-defun org-glance-headline:visit (&optional (headline (org-glance-headline:at-point)))
  "Visit HEADLINE by id. Grab source file from metastore."
  (let* ((file (org-glance-headline:file headline)))

    (if (file-exists-p file)
        (find-file file)
      (org-glance-db-outdated "File not found: %s" file))

    ;; we are now visiting headline file, let's remove restrictions
    (widen)

    ;; search for headline in buffer
    (org-glance-headline:search-buffer headline)

    ;; for interactive usage only
    (org-glance:expand-parents)
    (org-overview)
    (org-cycle 'contents)

    (org-glance-headline:at-point)))

(defmacro org-glance-headline:narrow (headline &rest forms)
  "Visit HEADLINE, narrow to its subtree and execute FORMS on it."
  (declare (indent 1) (debug t))
  `(let* ((file (org-element-property :file ,headline))
          (file-buffer (get-file-buffer file))
          (visited-buffer (current-buffer))
          result)

     (org-glance-headline:visit ,headline)

     (save-restriction
       (org-narrow-to-subtree)
       (when (= (point-max) (save-excursion
                              (org-end-of-meta-data)
                              (point)))
         (goto-char (point-max))
         (insert "\n"))
       (setq result (let ((org-link-frame-setup (cl-acons 'file 'find-file org-link-frame-setup)))
                      ,@forms)))

     (cond ((and file-buffer (not (eq file-buffer (current-buffer)))) (bury-buffer file-buffer))
           ((and file-buffer (eq file-buffer (current-buffer))) (progn (switch-to-buffer visited-buffer)
                                                                       (bury-buffer file-buffer)))
           (t (save-buffer)
              (kill-buffer (get-file-buffer file))))

     (save-buffer)

     result))

(cl-defun org-glance-headline:promote-to-the-first-level ()
  (org-glance:ensure-at-heading)
  (while (and (org-glance-headline-p) (looking-at "^\\*\\*"))
    (org-promote-subtree)))

(cl-defun org-glance-headline:contents (&optional (headline (org-glance-headline:at-point)))
  (condition-case nil
      (with-temp-buffer
        (org-mode)
        (insert-file-contents (org-glance-headline:file headline))
        (org-glance-headline:search-buffer headline)
        (org-narrow-to-subtree)
        (goto-char (point-min))
        (org-glance-headline:promote-to-the-first-level)
        (buffer-substring-no-properties (point-min) (point-max)))
    (error nil)))

(cl-defun org-glance-headline:links (&optional (headline (org-glance-headline:at-point)))
  (org-glance-headline:narrow headline
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
        (cons
         (substring-no-properties
          (or (nth 2 link)                            ;; link alias
              (org-element-property :raw-link link))) ;; full link if alias is none
         (org-element-property :begin link))))))

(cl-defun org-glance-headline:scan-file (&optional (file (buffer-file-name)))
  (with-temp-buffer
    (org-mode)
    (insert-file-contents file)
    (org-element-map (org-element-parse-buffer 'headline) 'headline
      (lambda (el)
        (when (org-glance-headline-p el)
          (-> el
            (org-glance-headline:enrich :file file)))))))

(cl-defun org-glance-headline:add-log-note (note &optional (headline (org-glance-headline:at-point)))
  (org-glance-headline:narrow (org-glance-headline:at-point)
    (goto-char (org-log-beginning t))
    (insert note "\n")
    (save-buffer)))

(cl-defun org-glance-headline:rename (title &optional (headline (org-glance-headline:at-point)))
  (org-glance-headline:narrow headline
    (let ((old-title (org-glance-headline:raw-value headline))
          (new-title (s-replace-regexp "[[:space:]]" " " title)))
      (org-glance-headline:add-log-note
       (org-glance:format "- Renamed from \"${old-title}\" to \"${new-title}\" on ${now}"))
      (org-glance-headline:goto-beginning-of-current-headline)
      (org-beginning-of-line)
      (org-kill-line)
      (insert new-title))))

(org-glance-module-provide)
