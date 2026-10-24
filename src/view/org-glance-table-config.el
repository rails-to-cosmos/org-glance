;;; org-glance-table-config.el --- JSON table layout records -*- lexical-binding: t -*-

;;; Commentary:
;; Native table layouts in the shared configuration file.

;;; Code:

(require 'cl-lib)
(require 'org-glance-config)
(require 'org-glance-config-file)
(require 'table-view)

(defconst org-glance-table-config--collections
  '(("tableViews" :columns :sort)
    ("tableColumns" :columns :hidden)
    ("tableRefs" :columns :hidden :order :sort :applied)))

(defun org-glance-table-config--fields (section)
  "Return the supported layout fields for SECTION."
  (or (cdr (assoc section org-glance-table-config--collections))
      (org-glance-config--error "Unknown native layout collection")))

(defun org-glance-table-config--strings (value)
  "Decode VALUE as an ordered list of column strings."
  (unless (and (vectorp value) (cl-every #'stringp value))
    (org-glance-config--error "Layout columns must be an array of strings"))
  (append value nil))

(defun org-glance-table-config--records (value identity)
  "Read VALUE as object records with unique string IDENTITY fields."
  (unless (vectorp value)
    (org-glance-config--error "Layout records must be an array"))
  (let ((seen (make-hash-table :test 'equal)))
    (mapc (lambda (record)
            (org-glance-config--object record)
            (let ((key (gethash identity record)))
              (unless (and (stringp key) (not (gethash key seen)))
                (org-glance-config--error "Layout record identities must be unique strings"))
              (puthash key t seen))) value))
  (append value nil))

(defun org-glance-table-config--decode (section object)
  "Decode a native layout OBJECT in SECTION."
  (org-glance-config--object object)
  (let (result)
    (dolist (field (org-glance-table-config--fields section))
      (let* ((key (substring (symbol-name field) 1))
             (value (gethash key object :missing)))
        (unless (eq value :missing)
          (setq result
                (plist-put
                 result field
                 (pcase field
                   (:columns
                    (if (equal section "tableViews")
                        (org-glance-table-config--strings value)
                      (mapcar
                       (lambda (column)
                         (org-glance-config--object column)
                         (let ((property (gethash "property" column))
                               (header (gethash "header" column)))
                           (unless (and (stringp property) (stringp header))
                             (org-glance-config--error "Custom column requires property and header"))
                           (cons property header)))
                       (org-glance-table-config--records value "property"))))
                   ((or :hidden :order) (org-glance-table-config--strings value))
                   (:sort
                    (mapcar
                     (lambda (sort)
                       (org-glance-config--object sort)
                       (let ((column (org-glance-config--field sort "column" #'stringp))
                             (direction (org-glance-config--field sort "direction"
                                          (lambda (v) (member v '("asc" "desc")))))
                             (nulls (org-glance-config--field sort "nulls"
                                      (lambda (v) (member v '("first" "last"))) "last")))
                         (unless (and column direction)
                           (org-glance-config--error "Sort requires column and direction"))
                         (table-view--make-sort-key column (equal direction "asc")
                                                    (intern nulls))))
                     (org-glance-table-config--records value "column")))
                   (:applied
                    (unless (numberp value)
                      (org-glance-config--error "Layout timestamp must be a number"))
                    value)))))))
    result))

(defun org-glance-table-config:encode (section value)
  "Encode native layout VALUE for SECTION as JSON data."
  (unless (proper-list-p value)
    (org-glance-config--error "Layout must be a finite property list"))
  (let ((fields (org-glance-table-config--fields section))
        (object (make-hash-table :test 'equal))
        (rest value))
    (while rest
      (unless (and (consp rest) (consp (cdr rest)) (memq (car rest) fields))
        (org-glance-config--error "Unsupported native layout value"))
      (let* ((field (pop rest)) (item (pop rest))
             (key (substring (symbol-name field) 1)))
        (unless (or (eq field :applied) (proper-list-p item))
          (org-glance-config--error "Layout field must be a finite list"))
        (unless (eq (gethash key object :missing) :missing)
          (org-glance-config--error "Duplicate native layout field"))
        (puthash
         key
         (pcase field
           (:columns
            (vconcat
             (if (equal section "tableViews") item
               (mapcar (lambda (column)
                         (unless (and (consp column) (stringp (car column))
                                      (stringp (cdr column)))
                           (org-glance-config--error "Unsupported custom column"))
                         (let ((entry (make-hash-table :test 'equal)))
                           (puthash "property" (car column) entry)
                           (puthash "header" (cdr column) entry)
                           entry)) item))))
           ((or :hidden :order) (vconcat item))
           (:sort
            (vconcat
             (mapcar
              (lambda (sort)
                (unless (and (consp sort) (stringp (car sort))
                             (memq (cdr sort) '(nil t asc-nulls-first desc-nulls-first)))
                  (org-glance-config--error "Unsupported native sort key"))
                (let ((entry (make-hash-table :test 'equal)))
                  (puthash "column" (table-view--sort-key-col sort) entry)
                  (puthash "direction" (if (table-view--sort-key-asc sort) "asc" "desc") entry)
                  (puthash "nulls" (symbol-name (table-view--sort-key-nulls sort)) entry)
                  entry)) item)))
           (:applied item))
         object)))
    (org-glance-table-config--decode section object)
    object))

(defun org-glance-table-config--collection (object section)
  "Return OBJECT's native layout SECTION, using an empty object if absent."
  (org-glance-table-config--fields section)
  (dolist (key (list "clients" "orgGlance" section))
    (setq object (org-glance-config--object
                  (gethash key object (make-hash-table :test 'equal)))))
  object)

(cl-defun org-glance-table-config:entries (snapshot &key section)
  "Read native SECTION entries from the observed JSON SNAPSHOT."
  (let (entries)
    (maphash (lambda (key value)
               (push (cons key (org-glance-table-config--decode section value)) entries))
             (org-glance-table-config--collection
              (org-glance-config-file:object snapshot) section))
    (sort entries (lambda (a b) (string< (car a) (car b))))))

(defun org-glance-table-config--extensions (encoded old known)
  "Preserve OLD's fields outside KNOWN in the replacement ENCODED object."
  (when (hash-table-p old)
    (maphash (lambda (name value)
               (unless (member name known) (puthash name value encoded))) old))
  encoded)

(defun org-glance-table-config--merge (section encoded old)
  "Preserve extensions of surviving records in SECTION's ENCODED layout."
  (org-glance-table-config--extensions
   encoded old (mapcar (lambda (field) (substring (symbol-name field) 1))
                      (org-glance-table-config--fields section)))
  (when (hash-table-p old)
    (dolist (field (if (equal section "tableViews") '("sort") '("columns" "sort")))
      (let* ((identity (if (equal field "columns") "property" "column"))
             (known (if (equal field "columns") '("property" "header")
                      '("column" "direction" "nulls")))
             (previous (make-hash-table :test 'equal)))
        (dolist (record (org-glance-table-config--records (gethash field old []) identity))
          (puthash (gethash identity record) record previous))
        (mapc (lambda (record)
                (let ((original (gethash (gethash identity record) previous)))
                  (org-glance-table-config--extensions record original known)
                  (when (and original (equal field "sort")
                             (eq (gethash "nulls" original :missing) :missing)
                             (equal (gethash "nulls" record) "last"))
                    (remhash "nulls" record))))
              (gethash field encoded [])))))
  encoded)

(cl-defun org-glance-table-config:write (root &key snapshot edits)
  "Apply native layout EDITS in ROOT at observed SNAPSHOT's revision.
Each edit is (SECTION KEY VALUE); nil VALUE removes that override."
  (unless (org-glance-config-file-p snapshot)
    (user-error "Native layouts require an observed configuration snapshot"))
  (org-glance-config-file:write
   root :expected (org-glance-config-file:revision snapshot)
   :change
   (lambda (object)
     (dolist (edit edits)
       (pcase-let ((`(,section ,key ,value) edit))
         (unless (stringp key) (org-glance-config--error "Layout key must be a string"))
         (org-glance-table-config--fields section)
         (if (null value)
             (remhash key (org-glance-table-config--collection object section))
           (let ((parent object))
             (dolist (name (list "clients" "orgGlance" section))
               (let ((child (gethash name parent :missing)))
                 (when (eq child :missing)
                   (setq child (make-hash-table :test 'equal))
                   (puthash name child parent))
                 (setq parent (org-glance-config--object child))))
             (puthash key (org-glance-table-config--merge
                           section (org-glance-table-config:encode section value)
                           (gethash key parent)) parent)))))
     (let ((candidate (org-glance-config-file--make :object object)))
       (dolist (collection org-glance-table-config--collections)
         (org-glance-table-config:entries candidate :section (car collection)))
       (org-glance-config:decode candidate))
     object)))

(provide 'org-glance-table-config)
;;; org-glance-table-config.el ends here
