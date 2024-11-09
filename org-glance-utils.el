(defun symbol-downcased-p (sym)
  "Return t if SYM is downcased (i.e., all lowercase), nil otherwise."
  (let ((name (symbol-name sym)))
    (string= name (downcase name))))

(provide 'org-glance-utils)
