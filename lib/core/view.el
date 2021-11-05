(require 'org-glance-module)

(org-glance:require
  org
  org-element
  subr-x

  lib.core.scope
  lib.models.Metastore
  lib.core.exceptions
  lib.models.Headline
  lib.utils.helpers)

(defvar org-glance-view-default-type '(all)
  "Default type for all views.")

(defun org-glance-view:ids ()
  "List registered views."
  (sort (hash-table-keys org-glance:classes) #'s-less?))

(cl-defmethod org-glance-view:metastore-location ((view org-glance-view))
  (let ((view-id (downcase (symbol-name (org-glance-view-id view)))))
    (f-join org-glance-directory
            view-id
            (format "%s.metadata.el" view-id))))

(cl-defmethod org-glance-view-filter ((view org-glance-view))
  (-partial
   #'(lambda (view headline)
       (when (-contains?
              (mapcar #'downcase (org-element-property :tags headline))
              (downcase (symbol-name (org-glance-view-id view))))
         headline))
   view))

(cl-defun org-glance-view:update (&optional (view-id (org-glance:choose-class)))
  (interactive)
  (unless (org-glance:get-class view-id)
    (when (y-or-n-p (format "Define new role? %s" view-id))
      (org-glance-def-view :id (intern view-id))))
  (let* ((view (org-glance:get-class view-id))
         (db (org-glance-view:metastore-location view))
         (filter (org-glance-view-filter view))
         (scope (or (org-glance-view-scope view) (list org-glance-directory)))
         (headlines (org-glance-scope-headlines scope filter)))
    (org-glance:log-info "Update view %s" view-id)
    (org-glance-metastore:create db headlines)
    (list view)))

(cl-defgeneric org-glance-view:headlines (view))

(cl-defmethod org-glance-view:headlines ((view null))
  "When VIEW is a symbol, extract org-glance-view from `org-glance-view` hashmap by key."
  nil)

(cl-defmethod org-glance-view:headlines ((view symbol))
  "When VIEW is a symbol, extract org-glance-view from `org-glance-view` hashmap by key."
  (org-glance-view:headlines (org-glance:get-class view)))

(cl-defmethod org-glance-view:headlines ((view string))
  "When VIEW is a string, extract org-glance-view from `org-glance-view` hashmap by key intern."
  (org-glance-view:headlines (org-glance:get-class (intern view))))

(cl-defmethod org-glance-view:headlines ((view list))
  "When VIEW is a list, apply org-glance-view:headlines for each element of it."
  (cl-loop for v in view
     append (org-glance-view:headlines v)))

(cl-defmethod org-glance-view:headlines ((view org-glance-view))
  "Browse each file of a VIEW scope, run org-element-map and collect headlines as org-elements."
  (org-glance-headlines
   :db (org-glance-view:metastore-location view)
   :scope (or (org-glance-view-scope view) (list org-glance-directory))
   :filter (org-glance-view-filter view)))

(cl-defmethod org-glance-view:headlines* ((view org-glance-view))
  "List headlines as formatted strings for VIEW."
  (->> view
    (org-glance-view:headlines)
    (mapcar #'org-glance-headline:title)
    (mapcar #'(lambda (hl) (format "[%s] %s" (org-glance-view-id view) hl)))))

(cl-defun org-glance-view-prompt (view action)
  "Generate prompt for VIEW. Assume ACTION context."
  (s-titleize (format "%s %s: " action (org-glance-view-id view))))

(cl-defun org-glance-view:completing-read (&optional (prompt "Choose view: "))
  "Run completing read PROMPT on registered views filtered by TYPE."
  (let ((views (org-glance-view:ids)))
    (if (> (length views) 1)
        (intern (org-completing-read prompt views))
      (car views))))

(cl-defun org-glance-def-view (&key
                                 id
                                 type
                                 scope
                                 &allow-other-keys)
  (let ((view (or (org-glance:get-class id)
                  (org-glance-view:create :id id
                                          :type type
                                          :scope scope))))

    (puthash id view org-glance:classes)

    (org-glance:log-info "View \"%s\"%s is now ready to glance %s"
             id
             (if type (concat " of type \"" (s-trim (pp-to-string type)) "\"") "")
             (if scope (concat " over scope \"" (s-trim (pp-to-string scope)) "\"") ""))

    view))

(cl-defun org-glance:choose-class (&optional (prompt "Choose view: "))
  (org-completing-read prompt (org-glance-view:ids)))

(org-glance:provide)
