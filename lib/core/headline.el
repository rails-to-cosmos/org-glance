(require 'org-glance-module)

(org-glance-module-import lib.utils.helpers)
(org-glance-module-import lib.core.metastore)

(cl-defun org-glance-headline:by-id (id)
  "Get org-element headline by ID."
  (let ((matched-headlines (cl-loop for vid in (org-glance-view:ids)
                              for metastore = (->> vid
                                                org-glance-view
                                                org-glance-view-metadata-location
                                                org-glance-metastore:read)
                              for headline = (gethash id metastore)
                              when headline
                              collect (org-glance-metastore:deserialize headline))))
    (unless matched-headlines
      (org-glance-headline-not-found "%s. Try to update view or make sure the headline was not deleted" id))
    (if (= (length matched-headlines) 1)
        (car matched-headlines)
      (car matched-headlines) ;; TODO Fix conflicts in DOCTOR method

      ;; (let ((conflicting-headlines (cl-loop for headline in matched-headlines
      ;;                                 collect (cons (format "%s at %d in file %s %s"
      ;;                                                       (org-glance-headline:title headline)
      ;;                                                       (org-glance-headline:begin headline)
      ;;                                                       (org-glance-headline:file headline)
      ;;                                                       headline)
      ;;                                               headline))))
      ;;   (alist-get
      ;;    (org-completing-read "ID collision detected. Please resolve it: " conflicting-headlines nil 'require-match)
      ;;    conflicting-headlines
      ;;    nil
      ;;    nil
      ;;    #'string=))
      )))

(cl-defun org-glance-headline:at-point ()
  "Get org-glance-headline from subtree at point.
Subtree must satisfy the requirements of `org-glance-headline-p'"
  (save-excursion
    (org-glance-headline:goto-beginning-of-nearest-headline)
    (org-glance-headline:by-id (org-glance-headline:id))))

(cl-defun org-glance-headline:search-buffer (headline)
  "Search buffer for HEADLINE and return its point.
Raise `org-glance-headline-not-found` error on fail.''"
  (let ((points (org-element-map (org-element-parse-buffer 'headline) 'headline
                  (lambda (hl) (when (org-glance-headline:eq hl headline)
                            (org-element-property :begin hl))))))
    (unless points
      (org-glance-headline-not-found "Headline not found in file %s: %s" file headline))

    (when (> (length points) 1)
      (warn "Headline ID %s is not unique in file %s"
            (org-glance-headline:id headline)
            (org-glance-headline:file headline)))

    (car points)))

(cl-defgeneric org-glance-headline:visit (headline)
  "Visit HEADLINE.")

(cl-defmethod org-glance-headline:visit ((headline symbol))
  "Visit HEADLINE by headline id symbol name."
  (org-glance-headline:visit (symbol-name headline)))

(cl-defmethod org-glance-headline:visit ((headline list))
  "Visit HEADLINE by headline id symbol name."
  (org-glance-headline:visit (org-glance-headline:id headline)))

(cl-defmethod org-glance-headline:visit ((headline string))
  "Visit HEADLINE by id."
  (let* ((headline (org-glance-headline:by-id headline))
         ;; extract headline filename
         (file (org-element-property :file headline))
         ;; cache file buffer
         (buffer (get-file-buffer file)))

    (cond ((file-exists-p file) (find-file file))
          (t (org-glance-db-outdated "File not found: %s" file)))

    ;; we are now at headline file, let's remove restrictions
    (widen)

    ;; search for headline in buffer
    (goto-char (org-glance-headline:search-buffer headline))
    (org-glance-headline:expand-parents)
    (org-overview)
    (org-cycle 'contents)))

(cl-defun org-glance-headline:visit-headline-at-point ()
  (interactive)
  (save-excursion
    (org-glance-headline:goto-first-level-headline)
    (org-glance-headline:visit (org-glance-headline:id))))

(defmacro org-glance-with-headline-narrowed (headline &rest forms)
  "Visit HEADLINE, narrow to its subtree and execute FORMS on it."
  (declare (indent 1) (debug t))
  `(let* ((file (org-element-property :file ,headline))
          (file-buffer (get-file-buffer file))
          (visited-buffer (current-buffer))
          res)
     (org-glance-headline:visit ,headline)
     (org-narrow-to-subtree)
     (unwind-protect
          (setq res (let ((org-link-frame-setup (cl-acons 'file 'find-file org-link-frame-setup)))
                      ,@forms))
       (widen))
     (cond ((and file-buffer (not (eq file-buffer (current-buffer)))) (bury-buffer file-buffer))
           ((and file-buffer (eq file-buffer (current-buffer))) (progn (switch-to-buffer visited-buffer)
                                                                       (bury-buffer file-buffer)))
           (t (kill-buffer (get-file-buffer file))))
     res))

(cl-defun org-glance-headline:beginning-of-the-first-level ()
  "Beginning of headline as a first-level heading."
  (when (org-at-heading-p)
    (save-excursion
      (org-beginning-of-line)
      (while (looking-at "*")
        (forward-char))
      (- (point) 1))))

(cl-defun org-glance-headline:contents (headline)
  (save-window-excursion
    (save-excursion
      (org-glance-headline:visit headline)
      (save-restriction
        (org-narrow-to-subtree)
        (buffer-substring-no-properties (org-glance-headline:beginning-of-the-first-level) (point-max))))))

(cl-defun org-glance-headline:links (headline)
  (org-glance-with-headline-narrowed headline
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
        (cons
         (substring-no-properties
          (or (nth 2 link)                            ;; link alias
              (org-element-property :raw-link link))) ;; full link if alias is none
         (org-element-property :begin link))))))

(org-glance-module-provide)
