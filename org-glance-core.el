(eval-when-compile
  (require 'cl)
  (require 'subr-x))

(eval-and-compile
  (require 'org)
  (require 'org-element)
  (require 'load-relative)
  (require 'org-glance-scope)
  (require 'org-glance-db))

(cl-defun org-glance-completing-read (headlines &key prompt)
  (org-completing-read prompt
                       (cl-loop for headline in headlines
                                collect (org-glance-format headline))))

(cl-defun org-glance-format (headline)
  (or (org-element-property :TITLE headline)
      (org-element-property :raw-value headline)))

(cl-defun org-glance-browse (headlines &key choice fallback)
  (or (cl-loop for headline in headlines
               when (string= (org-glance-format headline) choice)
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

(provide-me)
