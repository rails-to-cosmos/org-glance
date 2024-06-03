(require 'org-glance-module)

(org-glance:require
  org
  org-element
  subr-x

  lib.core.scope
  lib.core.metastore
  lib.core.exceptions
  lib.core.headline
  lib.utils.helpers)

;; TODO Move to controller
(defvar -org-glance-views (make-hash-table)
  "Hash table (id->view) that lists all registered tags.")

(defun org-glance-views:list ()
  "List registered views."
  (sort (hash-table-keys -org-glance-views) #'s-less?))

(cl-defstruct (org-glance-view (:constructor org-glance-view:create))
  "This structure contains metadata about categorized `org-mode' headlines."
  (id nil :type 'symbol :read-only t :documentation "Unique identifier for `org-glance-view'.")
  (type nil :type 'list :read-only nil :documentation "Determines list of actions allowed to use on headlines of this view.")
  (scope nil :type 'list :read-only nil :documentation "List of files where org-glance should search for headlines for this view."))

(cl-defmethod org-glance-tag:create ((tag symbol))
  (unless (org-glance-view:get tag))
  (let ((view (org-glance-view:create :id tag ))))
  (puthash tag () -org-glance-views))

(cl-defmethod org-glance-view:get ((tag symbol))
  (gethash tag -org-glance-views))

(cl-defmethod org-glance-view:id ((view org-glance-view))
  (downcase (symbol-name (org-glance-view-id view))))

(cl-defmethod org-glance-view:metastore ((view org-glance-view))
  (let ((view-id (org-glance-view:id view)))
    (f-join org-glance-directory view-id (format "%s.metadata.el" view-id))))

(cl-defmethod org-glance-view-filter ((view org-glance-view))
  (-partial
   #'(lambda (view headline)
       (when (-contains?
              (mapcar #'downcase (org-element-property :tags headline))
              (downcase (symbol-name (org-glance-view-id view))))
         headline))
   view))

(cl-defgeneric org-glance-view:headlines (view))

(cl-defmethod org-glance-view:headlines ((view null))
  "When VIEW is a symbol, extract org-glance-view from `org-glance-view` hashmap by key."
  nil)

(cl-defmethod org-glance-view:headlines ((view symbol))
  "When VIEW is a symbol, extract org-glance-view from `org-glance-view` hashmap by key."
  (org-glance-view:headlines (org-glance-view:get view)))

(cl-defmethod org-glance-view:headlines ((view string))
  "When VIEW is a string, extract org-glance-view from `org-glance-view` hashmap by key intern."
  (org-glance-view:headlines (org-glance-view:get (intern view))))

(cl-defmethod org-glance-view:headlines ((view list))
  "When VIEW is a list, apply org-glance-view:headlines for each element of it."
  (cl-loop for v in view
     append (org-glance-view:headlines v)))

(cl-defmethod org-glance-view:headlines ((view org-glance-view))
  "Browse each file of a VIEW scope, run org-element-map and collect headlines as org-elements."
  (org-glance-headlines
   :db (org-glance-view:metastore view)
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

(cl-defun org-glance-view:completing-read (&optional (prompt "Choose view: ") (require-match t))
  "Run completing read PROMPT on registered views filtered by TYPE."
  (let ((views (org-glance-views:list)))
    (if (= (length views) 1)
        (car views)
      (intern (completing-read prompt views nil require-match)))))

(cl-defun org-glance-def-view (&key id type scope &allow-other-keys)
  (let ((view (or (org-glance-view:get id)
                  (org-glance-view:create :id id
                                          :type type
                                          :scope scope))))

    (puthash id view -org-glance-views)

    (org-glance:log-info "View \"%s\"%s is now ready to glance %s"
                         id
                         (if type (concat " of type \"" (s-trim (pp-to-string type)) "\"") "")
                         (if scope (concat " over scope \"" (s-trim (pp-to-string scope)) "\"") ""))

    view))

(cl-defun org-glance:choose-class (&optional (prompt "Choose view: "))
  (completing-read prompt (org-glance-views:list) nil t))

(cl-defun org-glance-capture-template (class &key (default ""))
  (let ((class (if (symbolp class) class (intern class)))
        (capture-template-config-file (f-join (org-glance-overview:directory class) "capture-template.org")))
    (s-replace "%?" (concat default "%?")
               (cond ((f-exists-p capture-template-config-file) (with-temp-buffer
                                                                  (insert-file-contents capture-template-config-file)
                                                                  (buffer-substring-no-properties (point-min) (point-max))))
                     (t "* %?")))))

(org-glance:provide)
