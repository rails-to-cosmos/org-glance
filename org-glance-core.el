(cl-defun org-glance-completing-read (headlines &key prompt title-property)
  (org-completing-read prompt
                       (cl-loop for headline in headlines
                                collect (org-glance-format headline :title-property title-property))))

(cl-defun org-glance-format (headline &key title-property)
  (or (and title-property (org-element-property title-property headline))
      (org-element-property :raw-value headline)))

(cl-defun org-glance-browse (headlines &key choice fallback title-property)
  (or (cl-loop for headline in headlines
               when (string= (org-glance-format headline :title-property title-property) choice)
               do (cl-return headline))
      (when fallback (funcall fallback choice))))

(cl-defgeneric org-glance-read (file &key filter)
  "Read org-element headlines from one or many files.")

(cl-defmethod org-glance-read ((files list) &key filter)
  (cl-loop for file in (org-glance-scope--adapt files)
           do (message "Glance %s" file)
           append (org-glance-read file :filter filter) into result
           do (redisplay)
           finally (cl-return result)))

(cl-defmethod org-glance-read ((file string) &key filter)
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

(cl-defun org-glance-save (file entries &key title-property)
  (unless (file-exists-p (file-name-directory file))
    (make-directory (file-name-directory file) t))
  (with-temp-file file
    (insert "`(")
    (dolist (entry entries)
      (insert (org-glance-cache--serialize entry
                                    :title-property title-property) "\n"))
    (insert ")"))
  entries)

(cl-defun org-glance-load (file &key title-property)
  (let ((entries
         (with-temp-buffer (insert-file-contents file)
                           (->> (buffer-string)
                                substring-no-properties
                                read
                                eval))))
    (cl-loop for entry in entries
             collect (org-glance-cache--deserialize entry
                                                    :title-property title-property))))

(cl-defun org-glance-cache-reread (&key
                                   filter
                                   cache-file
                                   (scope '(agenda))
                                   (title-property :TITLE)
                                   &allow-other-keys)
  (let ((headlines (org-glance-read scope :filter filter)))

    (unless headlines
      (user-error "Nothing to glance at scope %s" (pp-to-string scope)))

    (when cache-file
      (org-glance-save cache-file headlines :title-property title-property))

    headlines))

(provide-me)
