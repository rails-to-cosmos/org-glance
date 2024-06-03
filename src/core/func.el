(require 'org-glance-module)

(eval-and-compile
  (cl-defmacro org-glance:interactive-lambda (&rest forms)
    "Define interactive lambda function with FORMS in its body."
    (declare (indent 0) (debug t))
    `(lambda () (interactive) ,@forms)))

(org-glance:provide)
