(require 'org-glance-module)

(org-glance:require
  eieio
  f
  lib.utils.helpers)

(defclass org-glance-metastore ()
  ((store :initarg :store
          :initform (make-hash-table)
          :type hash-table
          :protection private)
   (location :initarg :location
             :initform (make-temp-file "org-glance-metastore-")
             :type f-file))
  "Stores information about `org-glance-headline'.")

(cl-defmethod initialize-instance :after ((m org-glance-metastore) &rest _)
  "Constructor for `org-glance-metastore'."
  (org-glance-metastore-load m (oref m location)))

(cl-defmethod org-glance-metastore-save ((m org-glance-metastore) file)
  "Save metastore M to FILE."
  (with-temp-file (-org-glance:make-file-directory file)
    (insert (prin1-to-string (oref m store)))))

(cl-defmethod org-glance-metastore-load ((m org-glance-metastore) file)
  "Load metastore M from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (oset m store (read (buffer-substring-no-properties (point-min) (point-max))))))

(cl-defmethod org-glance-metastore-get ((m org-glance-metastore) key)
  "Get KEY from metastore M."
  (gethash key (oref m store)))

(cl-defmethod org-glance-metastore-set ((m org-glance-metastore) key value)
  "Set KEY to VALUE in metastore M."
  (puthash key value (oref m store))
  (org-glance-metastore-save m (oref m location)))

(cl-defmethod org-glance-metastore-remove ((m org-glance-metastore) key)
  "Remove KEY from metastore M."
  (remhash key (oref m store))
  (org-glance-metastore-save m (oref m location)))

(org-glance:provide)
