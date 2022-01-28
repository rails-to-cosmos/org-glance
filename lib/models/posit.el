(require 'org-glance-module)

;; Fact-based modeling
;;; https://en.wikipedia.org/wiki/Object-role_modeling

(cl-defun org-glance-posit:location ()
  (f-join org-glance-directory "posit.el"))

(cl-defun org-glance-posit:write (&rest posits)
  (cl-loop
     with eol = "\n"
     for posit in posits
     unless (null posit)
     collect (prin1-to-string posit) into lines
     finally
       (unless (file-exists-p (org-glance-posit:location))
         (-org-glance:make-file-directory (org-glance-posit:location))
         (make-empty-file (org-glance-posit:location)))
       (append-to-file (concat (s-join eol lines) eol) nil (org-glance-posit:location))))

(cl-defun org-glance-posit:read ()
  (with-temp-buffer
    (cl-loop
       initially
         (insert-file-contents (org-glance-posit:location))
         (goto-char (point-min))
       until
         (eobp)
       collect
         (read (buffer-substring (point) (line-end-position)))
       do
         (forward-line))))

(cl-defun org-glance-appearance (thing &optional role)
  "Create appearance of THING with ROLE."
  (cond ((listp thing) thing)
        (t (list thing role))))

(cl-defun org-glance-posit (&rest args &key (value t) &allow-other-keys)
  "Posit that appearances ARGS are related in context of VALUE.

Each arg of ARGS should be a proper `org-glance-appearance'.

By default, assume role of the first appearance is 'referrer, and the others are 'referee."
  (interactive)
  (let* ((kwargs (list :value))
         (appearances (--take-while (not (member it kwargs)) args))
         (value (if (member :value args)
                    (nth 1 (--drop-while (not (eq it :value)) args))
                  t)))
    (vector
     (apply #'list (--map (org-glance-appearance it 'relative) appearances))
     value
     (current-time))))

;; test cases:
;; (org-glance-posit (list 'Class 'is-class) 'a 'b 'c :value t)
;; (org-glance-posit (list 'Class 'is-class))
;; (org-glance-posit (list 'Class 'is-class) :value nil)
;; (org-glance-posit (list 'Class 'is-class) 'a :value nil)

;; 1. thing - unique identifier
;; 2. role - string representing a role
;; 2.5 appearance (dereferencing) set - ((thing, role), ...) bindings
;; 3. value - anything, default t

;; When to create posit?
;;; 1. Capture headline (INSERT).
;;; 2. Refer (UPDATE relations).
;;; 3. Materialize sync (UPDATE contents & relations)
;;; 4. Import headlines.

;;; Actions: materialize/open/extract/... ???

;; (org-glance-posit '(id001 created-at) :value "10 Aug 2020")
;; (org-glance-posit '(id001 title) :value "Some Interesting Article")

;; (org-glance-posit '(A is-class) :value "Article")
;; (org-glance-posit '(A is-class) :value "Webpage")

;; (org-glance-posit '(id001 thing) :value 'active '(Article class))
;; (org-glance-posit '(id001 thing) '(Article class) :value 'active)
;; (org-glance-posit '(id001 thing) '(Article class) :value nil)

;; (org-glance-posit '(id001 thing) '(A class) :value 'active)
;; (org-glance-posit '(id002 thing) '(A class) :value 'active)
;; (org-glance-posit 'id001 'id002 'id003 '(id004 role) :value 'active)

;; (cl-defun org-glance-posit:get-all-posits-of-thing (thing)) ;;; === get thing's logbook
;; (cl-defun org-glance-posit:get-all-roles-of-thing (thing)) ;;; === get all roles with non-nil value

;; (cl-defun org-glance-posit:get-all-relations-of-thing (thing)) ;;; ???
;; ;; + dfs, bfs, a*
;; ;; search datalog
;; ;; (cl-defun org-glance-posit:get-all-classes-of-thing (thing)) ;;; === get all relations with role 'class

;; (cl-defun org-glance-posit:get-all-things-of-class (class)) ;;; search by tag
;; (cl-defun org-glance-posit:get-all-things-with-role (role)) ;;; role=thing - get all things. role=class - get all classes

;; (cl-defun org-glance-posit:get-all-things-with-role-eq (role value))

;; (org-glance-posit:read)

;; (cl-defun org-glance-def-class (thing)
;;   "Reserve THING as a 'class."
;;   (org-glance-posit (list thing 'is-class)))

;; (cl-defun org-glance-thing (thing)
;;   "Set THING role to be equal 'thing."
;;   (list thing 'thing))

;; (cl-defun org-glance-class (class)
;;   "Set CLASS role to be equal 'class."
;;   (list class 'class))

(org-glance-provide)
