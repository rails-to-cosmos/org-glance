;; (cl-defun org-glance-store:filter-normalize (stmt)
;;   "Convert STMT to prefix notation.
;; Each substring is a run of \"valid\" characters, i.e., letters, colons for tags and AND/OR expressions.

;; - :TAG: words wrapped in colons match org-mode tag.
;; - TODO uppercase words match org-mode states.
;; - AND/OR expressions could be used.
;; - Use parentheses to prioritize expressions."
;;   (cl-loop
;;      with stack = (list)
;;      with output = (list)
;;      with operators = (a-list "|" 1
;;                               "&" 2
;;                               "=" 3)
;;      for c across-ref (->> stmt
;;                            (string-replace "&" "")
;;                            (string-replace "|" "")
;;                            (string-replace "=" "")
;;                            (s-replace "and" " & ")
;;                            (s-replace "or" " | ")
;;                            (s-replace-regexp " \\([A-Z]+\\)" "(state = \\1)")
;;                            (s-replace-regexp ":\\([a-zA-Z0-9]+\\):" "(class = \\1)")
;;                            (s-replace-regexp "[ ]+" " ")
;;                            downcase
;;                            s-trim
;;                            (format "(%s)")
;;                            reverse
;;                            (s-replace-all (a-list "(" ")" ")" "(")))
;;      for char = (string c)

;;      if
;;        (s-matches-p "^[:]?[a-zA-Z0-9 ]+[:]?$" char)
;;      do
;;        (push char output)

;;      else if
;;        (string= "(" char)
;;      do
;;        (push char stack)
;;        (push ")" output)

;;      else if
;;        (string= ")" char)
;;      do
;;        (while (and stack (not (string= (car stack) "(")))
;;          (push (concat (pop stack) " ") output)
;;          (push "(" output))
;;        (pop stack) ;; remove '('

;;      else if
;;        (a-get operators char) ;; operator found
;;      do
;;        (while (< (a-get operators char 0)
;;                  (a-get operators (car stack) 0))
;;          (push (concat (pop stack) " ") output)
;;          (push "(" output))
;;        (push char stack)

;;      finally do
;;        (while stack
;;          (push (concat (pop stack) " ") output))

;;      finally return (s-join "" output)))

;; (org-glance-store:filter-normalize ":a1:  AND :b2: OR CANCELLED AND DONE OR TODO")

;; (apply #'string (org-glance-store:filter-tokenize ":a1:  AND :b2: OR CANCELLED"))

;; (-reduce #'list (org-glance-store:filter-tokenize ":a1: AND :b2: OR CANCELLED AND DONE"))
;; (string (aref "aello" 0))

;; (org-glance-store:filter-tokenize ":a1:")

;; (s-match-strings-all "([[:word:][:blank:]:]+)" ":a1: AND :b2: AND (:c3: OR (TODO AND DONE OR :d3:)")
;; (s-match-strings-all ":[[:word:]_]+:" ":a1: AND :b2: AND (:c3: OR (:c2: AND DONE)")

;; (org-glance-store:filter-tokenize "Hello")
