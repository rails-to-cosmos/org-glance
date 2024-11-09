(require 'org)
(require 'org-element)

;; TODO remove optionals

(cl-defun org-glance-headline? (headline)
  "Assume HEADLINE is an `org-element' with :ORG_GLANCE_ID property specified.
Return headline or nil if it is not a proper `org-glance-headline'."
  (when (org-element-property :ORG_GLANCE_ID headline)
    headline))

(cl-defun org-glance-headline:update (element &rest properties)
  "Enrich `org-element' ELEMENT with PROPERTIES."
  (cl-loop for (key value) on properties by #'cddr
           do (org-element-put-property element key value)
           finally (return element)))

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

(provide 'org-glance-headline)
