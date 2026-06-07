;;; test-helpers.el --- Helpers for org-glance
(require 'ert)
(require 's)
(require 'org-glance)

(cl-defmacro with-temp-directory (dir &rest body)
  "Create a temporary directory, bind it to DIR, run BODY in it, and delete the directory afterward.
DIR is a symbol that will hold the path to the temporary directory within BODY."
  (declare (indent 1))
  `(let ((,dir (make-temp-file "temp-dir-" t)))
     (unwind-protect
         (progn ,@body)
       (delete-directory ,dir t))))

(cl-defmacro org-glance-test:session (&rest body)
  (declare (indent 0))
  `(with-temp-directory org-glance-directory
     (org-glance-init org-glance-directory)
     ,@body))

(cl-defmacro org-glance-test:with-graph (graph &rest body)
  "Create a graph in a fresh temp directory, bind it to GRAPH, run BODY."
  (declare (indent 1))
  `(with-temp-directory dir
     (let ((,graph (org-glance-graph dir)))
       ,@body)))

(cl-defun org-glance-test:headline (id &rest lines)
  "Build an `org-glance-headline' carrying ID.
LINES is the heading, then optional planning (SCHEDULED:/DEADLINE:/CLOSED:)
lines, then optional body.  The ORG_GLANCE_ID drawer is placed after the
heading and any planning lines -- where org expects a property drawer --
so the id parses correctly whether or not a body or planning is present."
  (let* ((rest (cdr lines))
         (planning (seq-take-while
                    (lambda (l) (string-match-p "^\\(SCHEDULED\\|DEADLINE\\|CLOSED\\):" l))
                    rest))
         (body (seq-drop rest (length planning))))
    (apply #'org-glance-headline--from-lines
           (append (list (car lines))
                   planning
                   (list ":PROPERTIES:"
                         (format ":ORG_GLANCE_ID: %s" id)
                         ":END:")
                   body))))

(provide 'test-helpers)
;;; test-helpers.el ends here
