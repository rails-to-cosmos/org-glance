(cl-defun org-glance-serialize (headline)
  (prin1-to-string
   (list (when org-glance-title-property
           (org-element-property org-glance-title-property headline))
         (org-element-property :raw-value headline)
         (org-element-property :begin headline)
         (org-element-property :file headline))))

(cl-defun org-glance-deserialize (input)
  (destructuring-bind (alias title begin file) input
    (org-element-create 'headline
       `(,org-glance-title-property ,alias
                                    :raw-value ,title
                                    :begin ,begin
                                    :file ,file))))

(defun org-glance-visit (file headline)
  (find-file file)
  (goto-char (org-element-property :begin headline))
  (org-show-context 'org-goto))

(defun org-glance-format (headline)
  (or (when org-glance-title-property
        (org-element-property org-glance-title-property headline))
      (org-element-property :raw-value headline)))

(defun org-glance-browse (headlines &optional fallback)
  (let* ((prompt org-glance-prompt)
         (choice (org-completing-read prompt (mapcar #'org-glance-format headlines)))
         (headline (loop for headline in headlines
                         when (string= (org-glance-format headline) choice)
                         do (return headline))))
    (or headline (when fallback
                   (funcall fallback choice)))))

(cl-defgeneric org-glance-read (file &optional filter)
  "Read org-element headlines from one or many files.")

(cl-defmethod org-glance-read ((files list) &optional filter)
  (loop for file in (org-glance-adapt-scope files)
        do (message "Glance %s" file)
        append (org-glance-read file filter) into result
        when (not (sit-for 0))
        do (return result)
        finally (return result)))

(cl-defmethod org-glance-read ((file string) &optional filter)
  (pcase-let ((`(,file ,id) (s-split-up-to "#" file 2)))
    (when (and (file-exists-p file)
               (not (f-directory? file)))
      (with-temp-buffer
        (insert-file-contents file)

        (when id
          (goto-char (org-find-entry-with-id id))
          (org-narrow-to-subtree))

        (org-element-map (org-element-parse-buffer 'headline) 'headline
          (lambda (headline)
            (when-let (headline (if filter
                                    (when (funcall filter headline)
                                      headline)
                                  headline))
              (plist-put (cadr headline) :file file)
              headline)))))))

(cl-defun org-glance-save (file entries)
  (unless (file-exists-p (file-name-directory file))
    (make-directory (file-name-directory file) t))

  (with-temp-file file
    (insert "`(")
    (dolist (entry entries)
      (insert (org-glance-serialize entry) "\n"))
    (insert ")"))

  entries)

(cl-defun org-glance-load (file)
  (let ((entries (with-temp-buffer
                   (insert-file-contents file)
                   (eval (read (substring-no-properties (buffer-string)))))))
    (mapcar #'org-glance-deserialize entries)))

(provide 'org-glance-entry)
