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

(cl-defun org-glance-headline:search-backward ()
  (org-glance:ensure-at-heading)
  (while (and (not (org-glance-headline-p))
              (> (point) (point-min)))
    (org-up-heading-or-point-min))
  (org-glance-headline-p))

(cl-defun org-glance-headline:at-point ()
  "Build `org-glance-headline' from `org-element' at point.
If point is inside subtree, search backward for the first occurence of `org-glance-headline'."
  (save-excursion
    (-some-> (org-glance-headline:search-backward)
      (org-element-put-property :file (buffer-file-name))
      (org-element-put-property :indent (org-glance:indent-level)))))

(cl-defun org-glance-headline:id (&optional (headline (org-glance-headline:at-point)))
  "Return unique identifer of HEADLINE."
  (org-element-property :ORG_GLANCE_ID headline))

(cl-defun org-glance-headline:state (&optional (headline (org-glance-headline:at-point)))
  (substring-no-properties (org-element-property :todo-keyword headline)))

(cl-defun org-glance-headline:commented? (&optional (headline (org-glance-headline:at-point)))
  (org-element-property :commentedp headline))

(cl-defun org-glance-headline:title (&optional (headline (org-glance-headline:at-point)))
  (or (org-element-property :TITLE headline)
      (org-element-property :raw-value headline)))

(cl-defun org-glance-headline:modtime (&optional (headline (org-glance-headline:at-point)))
  (-> headline
    org-glance-headline:file
    file-attributes
    file-attribute-modification-time
    (format-time-string "%Y-%m-%d %H:%M:%S")))

(cl-defun org-glance-headline:file (&optional (headline (org-glance-headline:at-point)))
  (org-element-property :file headline))

(cl-defun org-glance-headline:indent (&optional (headline (org-glance-headline:at-point)))
  (org-element-property :indent headline))

(cl-defun org-glance-headline:buffer (&optional (headline (org-glance-headline:at-point)))
  (get-file-buffer (org-glance-headline:file headline)))

(cl-defun org-glance-headline:begin (&optional (headline (org-glance-headline:at-point)))
  (org-element-property :begin headline))

(cl-defun org-glance-headline:view-ids (&optional (headline (org-glance-headline:at-point)))
  (mapcar #'s-titleized-words (org-element-property :tags headline)))

(cl-defun org-glance-headline:eq (headline &optional (other (org-glance-headline:at-point)))
  (string= (org-glance-headline:id headline)
           (org-glance-headline:id other)))

(cl-defun org-glance-headline:search-buffer (&optional (headline (org-glance-headline:at-point)))
  (let ((points (org-element-map (org-element-parse-buffer 'headline) 'headline
                  (lambda (elem) (when (org-glance-headline:eq elem headline)
                              (org-element-property :begin elem))))))
    (unless points
      (org-glance-exception:headline-not-found "Headline not found in file %s: %s" file headline))

    (when (> (length points) 1)
      (warn "Headline ID %s is not unique in file %s"
            (org-glance-headline:id headline)
            (org-glance-headline:file headline)))

    (goto-char (car points))))

(cl-defun org-glance-headline:visit (&optional (headline (org-glance-headline:at-point)))
  "Visit HEADLINE by id. Grab source file from metastore."
  (let* ((file (org-glance-headline:file headline))
         (buffer (org-glance-headline:buffer headline)))

    (if (file-exists-p file)
        (find-file file)
      (org-glance-db-outdated "File not found: %s" file))

    ;; we are now visiting headline file, let's remove restrictions
    (widen)

    ;; search for headline in buffer
    (org-glance-headline:search-buffer headline)
    (org-glance:expand-parents)
    (org-overview)
    (org-cycle 'contents)))

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
       (setq result (let ((org-link-frame-setup (cl-acons 'file 'find-file org-link-frame-setup)))
                   ,@forms)))

     (cond ((and file-buffer (not (eq file-buffer (current-buffer)))) (bury-buffer file-buffer))
           ((and file-buffer (eq file-buffer (current-buffer))) (progn (switch-to-buffer visited-buffer)
                                                                       (bury-buffer file-buffer)))
           (t (kill-buffer (get-file-buffer file))))

     result))

(cl-defun org-glance-headline:contents (&optional (headline (org-glance-headline:at-point)))
  (condition-case nil
      (with-temp-buffer
        (org-mode)
        (insert-file-contents (org-glance-headline:file headline))
        (org-glance-headline:search-buffer headline)
        (org-narrow-to-subtree)
        (goto-char (point-min))
        (org-glance:promote-to-first-level)
        (buffer-substring-no-properties (point-min) (point-max)))
    (error nil)))

(cl-defun org-glance-headline:links (headline)
  (org-glance-headline:narrow headline
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
        (cons
         (substring-no-properties
          (or (nth 2 link)                            ;; link alias
              (org-element-property :raw-link link))) ;; full link if alias is none
         (org-element-property :begin link))))))

(org-glance-module-provide)
