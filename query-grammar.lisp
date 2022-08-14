(defparameter *grammar*
  '((class -> (Task Story Epic))
    (state -> (TODO DONE CANCELLED))
    (operator -> (AND OR))
    (query -> (class operator state))))

(defun rule-lhs (rule)
  "LHS of a rule."
  (first rule))

(defun rule-rhs (rule)
  "RHS of a rule."
  (rest (rest rule)))

(defun rewrites (category)
  "Rewrites of category."
  (rule-rhs (assoc category *grammar*)))

(defun generate (phrase)
  "Generate a random sentence or phrase"
  (if (listp phrase)
      (loop for word in phrase
            append (generate word))
      (let ((choices (rewrites phrase)))
        (if (null choices)
            (list phrase)
            (generate (random-elt choices))))))
