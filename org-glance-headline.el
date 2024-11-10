(require 'dash)
(require 'org)
(require 'org-element)

(require 'org-glance-utils)

(defvar org-glance-headline:spec `((:raw-value   . (:reader org-glance:headline-title        :writer org-glance:headline-title))
                                   (:begin       . (:reader org-glance-headline:begin        :writer org-glance-headline:begin))
                                   (:file        . (:reader org-glance-headline:file-name    :writer org-glance-headline:file-name))
                                   (:commentedp  . (:reader org-glance-headline:commented?   :writer org-glance-headline:commented?))
                                   (:archivedp   . (:reader org-glance-headline:archived?    :writer org-glance-headline:archived?))
                                   (:linked      . (:reader org-glance-headline:linked?      :writer org-glance-element:linked?))
                                   (:propertized . (:reader org-glance-headline:propertized? :writer org-glance-element:propertized?))
                                   (:encrypted   . (:reader org-glance-headline:encrypted?   :writer org-glance-element:encrypted?))
                                   (:buffer      . (:reader org-glance-headline:buffer       :writer org-glance-element:buffer))
                                   (:closed      . (:reader org-glance-headline:closed?      :writer org-glance-headline:closed?)))
  "Map `org-element-property' to `org-glance' extractor method.")

(cl-defun org-glance-headline:update (element &rest properties)
  "Enrich `org-element' ELEMENT with PROPERTIES."
  (cl-loop for (key value) on properties by #'cddr
           do (org-element-put-property element key value)
           finally (return element)))

(cl-defun org-glance-headline:serialize (headline)
  (cl-loop for (_ . methods) in org-glance-headline:spec
           for reader = (plist-get methods :reader)
           collect (funcall reader headline)))

(cl-defun org-glance-headline:deserialize (value)
  (cl-loop with element = (org-element-create 'headline)
           for (property . _) in org-glance-headline:spec
           for index from 0
           do (org-glance-headline:update element property (nth index value))
           finally (return element)))

(cl-defun org-glance-element:linked? (element)
  (and (org-glance-headline? element)
       (org-glance-headline:with-narrowed-headline element
         (not (null (org-glance--parse-links))))))

(cl-defun org-glance-element:propertized? (element)
  (and (org-glance-headline? element)
       (org-glance-headline:with-narrowed-headline element
         (org-end-of-meta-data t)
         (not (null (re-search-forward org-glance:key-value-pair-re nil t))))))

(cl-defun org-glance-element:encrypted? (element)
  (and (org-glance-headline? element)
       (org-glance-headline:with-narrowed-headline element
         (org-end-of-meta-data t)
         (not (null (looking-at "aes-encrypted V [0-9]+.[0-9]+-.+\n"))))))

(cl-defun org-glance-element:buffer (element)
  (and (org-glance-headline? element)
       (condition-case nil
           (buffer-name (get-file-buffer (org-glance-headline:file-name headline)))
         (wrong-type-argument nil))))

(cl-defun org-glance-headline? (headline)
  "Assume HEADLINE is an `org-element' with :ORG_GLANCE_ID property specified.
Return headline or nil if it is not a proper `org-glance-headline'."
  (when (org-element-property :ORG_GLANCE_ID headline)
    headline))

(cl-defun org-glance-headline:title (headline)
  (or (org-element-property :TITLE headline)
      (org-element-property :raw-value headline)
      ""))

(cl-defun org-glance-headline:begin (headline)
  (org-element-property :begin headline))

(cl-defun org-glance-headline:end (headline)
  (org-element-property :contents-end headline))

(cl-defun org-glance-headline:tags (headline)
  (mapcar #'org-glance-tag:from-string (org-element-property :tags headline)))

(cl-defun org-glance-headline:encrypted? (headline)
  (not (null (org-element-property :encrypted headline))))

(cl-defun org-glance-headline:linked? (headline)
  (not (null (org-element-property :linked headline))))

(cl-defun org-glance-headline:propertized? (headline)
  (not (null (org-element-property :propertized headline))))

(cl-defun org-glance-headline:active? (headline)
  (and (org-glance-headline? headline)
       (not (org-glance-headline:done? headline))
       (not (org-glance-headline:commented? headline))
       (not (org-glance-headline:archived? headline))
       (not (org-glance-headline:closed? headline))))

(cl-defun org-glance-headline:id (headline)
  "Return unique identifer of HEADLINE."
  (org-element-property :ORG_GLANCE_ID headline))

(cl-defun org-glance-headline:state (headline)
  (substring-no-properties (or (org-element-property :todo-keyword headline) "")))

(cl-defun org-glance-headline:commented? (headline)
  (not (null (org-element-property :commentedp headline))))

(cl-defun org-glance-headline:archived? (headline)
  (not (null (org-element-property :archivedp headline))))

(cl-defun org-glance-headline:closed? (headline)
  (not (null (org-element-property :closed headline))))

(cl-defun org-glance-headline:done? (headline)
  (member (org-glance-headline:state headline) org-done-keywords-for-agenda))

(cl-defun org-glance-headline:alias (headline)
  "Get title of HEADLINE considering alias property."
  (or (org-element-property :ALIAS headline)
      (org-element-property :TITLE headline)
      (org-element-property :raw-value headline)
      ""))

(cl-defun org-glance-headline:priority (headline)
  (org-element-property :priority headline))

(cl-defun org-glance-headline:creation-time (headline)
  (org-element-property :ORG_GLANCE_CREATION_TIME headline))

(cl-defun org-glance-headline:file-name (headline)
  (when-let (file (if (plist-member (nth 1 headline) :file)
                      (org-element-property :file headline)
                    (buffer-file-name)))
    (abbreviate-file-name file)))

(cl-defun org-glance-headline:modtime (headline)
  (-some-> headline
    org-glance-headline:file-name
    file-attributes
    file-attribute-modification-time
    (format-time-string "%Y-%m-%d %H:%M:%S")))

(cl-defun org-glance-headline:level (headline)
  (org-element-property :level headline))

(cl-defun org-glance-headline:buffer (headline)
  (let ((buffer (org-element-property :buffer headline)))
    (cond ((bufferp buffer) (buffer-name buffer))
          (t buffer))))

(cl-defun org-glance-headline:schedule (headline)
  (org-element-property :scheduled headline))

(cl-defun org-glance-headline:deadline (headline)
  (org-element-property :deadline headline))

;; TODO check it for correctness
(cl-defun org-glance-headline:search-parents ()
  "Traverse parents in search of a proper `org-glance-headline'."
  (org-glance--back-to-heading)

  (while (not (or ;; (org-glance-headline? (org-element-at-point))
               (org-before-first-heading-p)
               (bobp)))
    (org-up-heading-or-point-min))

  (-some-> (org-element-at-point)
    (org-glance-headline?)
    (org-glance-headline:from-element)))

(cl-defun org-glance-headline:at-point ()
  "Search for the first occurence of `org-glance-headline' in parent headlines."
  (save-excursion
    (when (condition-case nil
              (or (org-at-heading-p) (org-back-to-heading))
            (error nil))
      (org-glance-headline:search-parents))))

(cl-defmacro org-glance-headline:with-headline-at-point (&rest forms)
  `(save-excursion
     (org-glance-headline:search-parents)
     (unless (org-glance-headline? (org-glance-headline:at-point))
       (error "Unable to find headline at point"))
     (save-restriction
       (org-narrow-to-subtree)
       ,@forms)))

(cl-defmacro org-glance-headline:with-narrowed-headline (headline &rest forms)
  "Visit HEADLINE, narrow to its subtree and execute FORMS on it."
  (declare (indent 1) (debug t))
  `(let ((org-link-frame-setup (cl-acons 'file 'find-file org-link-frame-setup))
         (id (org-glance-headline:id ,headline))
         (file (org-glance-headline:file-name ,headline))
         (buffer (org-glance-headline:buffer ,headline)))
     (cond (file (org-glance--with-file-visited file
                   (org-glance-headline:search-buffer-by-id id)
                   (org-glance-headline:with-headline-at-point ,@forms)))
           ((and buffer (buffer-live-p buffer))
            (with-current-buffer buffer
              (org-glance-headline:search-buffer-by-id id)
              (org-glance-headline:with-headline-at-point ,@forms)))
           (t (HEADLINE-NOT-FOUND (prin1-to-string ,headline))))))

(cl-defun org-glance-headline:from-element (element)
  (when (eql 'headline (org-element-type element))
    (cl-loop for (property . methods) in org-glance-headline:spec
             for index from 0
             for writer = (plist-get methods :writer)
             for value = (funcall writer element)
             do (org-glance-headline:update element property value)
             finally (return element))))

(provide 'org-glance-headline)
