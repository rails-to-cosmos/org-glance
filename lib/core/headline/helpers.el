(require 'org-glance-module)

(org-glance-module-import lib.core.headline)
(org-glance-module-import lib.core.metastore)

(cl-defgeneric org-glance-headline:visit (headline)
  "Visit HEADLINE.")

(cl-defmethod org-glance-headline:visit ((id symbol))
  "Visit HEADLINE by headline id symbol name. Grab source file from metastore."
  (org-glance-headline:visit (symbol-name id)))

(cl-defmethod org-glance-headline:visit ((id list))
  "Visit HEADLINE by headline id symbol name. Grab source file from metastore."
  (org-glance-headline:visit (org-glance-headline:id id)))

(cl-defmethod org-glance-headline:visit ((id string))
  "Visit HEADLINE by id. Grab source file from metastore."
  (org-glance-headline:visit* (org-glance-metastore:get-headline-by-id id)))

(defmacro org-glance-headline:narrow (headline &rest forms)
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

(cl-defun org-glance-headline:promote-to-first-level ()
  (org-glance:ensure-at-heading)
  (while (and (org-glance-headline-p) (looking-at "^\\*\\*"))
    (org-promote-subtree)))

(cl-defun org-glance-headline:contents (headline)
  (condition-case nil
      (with-temp-buffer
        (org-mode)
        (insert-file-contents (org-glance-headline:file headline))
        (org-glance-headline:search-buffer headline)
        (org-narrow-to-subtree)
        (goto-char (point-min))
        (org-glance-headline:promote-to-first-level)
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
