(require 'org-glance-module)

(defvar org-glance-posit:location (f-join org-glance-directory "posit.el"))

(cl-defun org-glance-posit:write (posit)
  (append-to-file (concat (prin1-to-string posit) "\n") nil org-glance-posit:location))

(cl-defun org-glance-posit:read ()
  (with-temp-buffer
    (cl-loop
       initially
         (insert-file-contents org-glance-posit:location)
         (goto-char (point-min))
       until
         (eobp)
       collect
         (read (buffer-substring (point) (line-end-position)))
       do
         (forward-line))))

;; (cl-defun org-glance-posit:set-class (thing class &key (value t))
;;   "Posit THING has CLASS.  Set value of this posit to VALUE."
;;   ;; [{(Archie, thing), (Person, class)}, active, 1972-08-20]
;;   (interactive)
;;   (assert (booleanp value))
;;   (vector (current-time) value (list (list thing 'thing) (list class 'class))))

;; (org-glance-posit:set-role "C1" 'is-class :value "Infant")

;; (org-glance-posit:set-role "C2" 'is-class :value "Teenager")
;; (org-glance-posit:set-relation "A" "C1" :referrer 'thing :referee 'class :value t)


;; Fact-based modeling
;;; https://en.wikipedia.org/wiki/Object-role_modeling

(cl-defun org-glance-posit (&rest appearances &key (value t value-specified-p) &allow-other-keys)
  "Posit APPEARANCES are related in context of VALUE.

Referrer could be either list of two elements (id and role) or
one element (then implicitly assume its role as a `referrer').
Same logic applies to each referee."
  ;; [{(Archie, husband), (Bella, wife)}, married, 2004-06-19]
  (interactive)
  (cl-labels ((appearance (thing &optional role) (cond ((listp thing) thing) (t (list thing role)))))
    (let ((appearances (--take-while (not (member it (list :value))) appearances))
          (value (if value-specified-p
                     value
                   (nth 1 (--drop-while (not (member it (list :value))) appearances)))))
      (vector
       (apply #'list (--map (appearance it 'relative) appearances))
       value
       (current-time)))))

;; 1. thing - unique identifier
;; 2. role - string representing a role
;; 2.5 appearance (dereferencing) set - ((thing, role), ...) bindings
;; 3. value - anything, default t

(org-glance-posit '(id001 created-at) :value "10 Aug 2020")
(org-glance-posit '(id001 title) :value "Some Interesting Article")

(org-glance-posit '(A is-class) :value "Article")
(org-glance-posit '(A is-class) :value "Webpage")

(org-glance-posit '(id001 thing) :value 'active '(Article class))
(org-glance-posit '(id001 thing) '(Article class) :value 'active)
(org-glance-posit '(id001 thing) '(Article class) :value nil)

(org-glance-posit '(id001 thing) '(A class) :value 'active)
(org-glance-posit '(id002 thing) '(A class) :value 'active)
(org-glance-posit 'id001 'id002 'id003 '(id004 role) :value 'active)

(cl-defun org-glance-posit:get-all-posits-of-thing (thing)) ;;; === get thing's logbook
(cl-defun org-glance-posit:get-all-roles-of-thing (thing)) ;;; === get all roles with non-nil value

(cl-defun org-glance-posit:get-all-relations-of-thing (thing)) ;;; ???
;; + dfs, bfs, a*
;; search datalog
;; (cl-defun org-glance-posit:get-all-classes-of-thing (thing)) ;;; === get all relations with role 'class

(cl-defun org-glance-posit:get-all-things-of-class (class)) ;;; search by tag
(cl-defun org-glance-posit:get-all-things-with-role (role)) ;;; role=thing - get all things. role=class - get all classes

(cl-defun org-glance-posit:get-all-things-with-role-eq (role value))

(org-glance-posit:read)

(org-glance:provide)
