(cl-defgeneric org-glance-cardinality (object)
  "Return number of elements in OBJECT.")

(cl-defgeneric org-glance-import (scope)
  "Extract objects from SCOPE.")

(cl-defgeneric org-glance-export (obj dest)
  "Export OBJ to DEST.")

(cl-defgeneric org-glance-serialize (object)
  "Serialize OBJECT.")

(cl-defgeneric org-glance-save (obj dest)
  "Save OBJ to DEST.")

(cl-defgeneric org-glance-hash (object)
  "Get hash of OBJECT.")

(cl-defgeneric org-glance-title (object)
  "Get title of OBJECT.")

(cl-defgeneric org-glance-headlines (object)
  "Retrieve list of `org-glance-headline' instances from OBJECT.")

(cl-defgeneric org-glance-materialize (source target)
  "Materialize SOURCE to TARGET.
After materialiation calling to `org-glance-material-mode-commit' from TARGET should be applied to SOURCE.")

(cl-defgeneric org-glance-equal-p (obj1 obj2)
  "Return t if OBJ1 equals OBJ2.")

(provide 'org-glance-generic)
