(require 'org-glance-module)

(defclass org-glance-headline ()
  ((id :initarg :id
       :initform (intern (concat
                          (s-join "-" (mapcar #'number-to-string (current-time)))
                          (secure-hash 'md5 (buffer-string))))
       :type symbol
       :documentation "Unique symbol.")
   (class :initarg :class
          :type org-glance-class
          :documentation "Headline class.")
   (title :initarg :title
          :type string
          :documentation "The title of headline.")
   (contents :initarg :contents
             :initform ""
             :type string
             :documentation "Headline contents."))
  "A base class for tracking headlines.")

(cl-defmethod initialize-instance :after ((m org-glance-headline) &rest _)
  "Constructor for `org-glance-headline'."
  (cond ((not (slot-boundp m 'class)) (error "Unable to initialize `org-glance-headline': CLASS should be specified"))
        ((not (slot-boundp m 'title)) (error "Unable to initialize `org-glance-headline': TITLE should be specified"))))

(cl-defun org-glance-headline:create-headlines-from-element-at-point ()
  "Create `org-glance-headline' from `org-element' at point."
  (save-excursion
    (org-glance-ensure-at-heading)

    (let ((element (org-element-at-point)))
      (unless (eql 'headline (org-element-type element))
        (user-error "Unable to create `org-glance-headline' from `%s': `headline' expected" (org-element-type element)))

      (cl-loop for tag in (org-element-property :tags element)
         collect (org-glance-headline
                  :title (with-temp-buffer
                           (insert (or (org-element-property :TITLE element)
                                       (org-element-property :raw-value element)
                                       ""))
                           (org-glance-replace-links-with-titles)
                           (buffer-string))
                  :contents (let ((contents (s-trim (buffer-substring-no-properties
                                                     (org-element-property :begin element)
                                                     (org-element-property :end element)))))
                              (with-temp-buffer
                                (org-mode)
                                (save-excursion
                                  (insert contents))
                                (org-set-tags (format "%s" (s-downcase tag)))
                                (while (looking-at "^\\*\\*")
                                  (org-promote-subtree))
                                (buffer-substring-no-properties (point-min) (point-max))))
                  :class (org-glance-class :id (intern (s-downcase tag))))))))

(org-glance:provide)
