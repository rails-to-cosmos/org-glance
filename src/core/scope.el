(require 'org-glance-module)

(org-glance:require
  dash
  org

  src.core.headline
  src.utils.helpers)

(defvar org-glance-scope:extensions
  '("org" "org_archive"))

(defvar org-glance-scope--default-scope-alist
  '((file-with-archives . -org-glance:file-with-archives)
    (agenda . org-agenda-files)
    (agenda-with-archives . -org-glance:agenda-with-archives)))

(cl-defgeneric org-glance-scope (_)
  "Convert input to list of files if possible.")

(cl-defmethod org-glance-scope ((file string))
  "Return list of file S if exists."
  (let ((files (cond
                ((not (file-exists-p file)) (org-glance:log-warning "File \"%s\" does not exist" file) nil)
                ((not (file-readable-p file)) (org-glance:log-warning "File \"%s\" is not readable" file) nil)
                ((f-directory? file) (org-glance-scope (directory-files-recursively file "\\.*.org\\.*")))
                ((with-temp-buffer
                   (insert-file-contents file)
                   (hack-local-variables)
                   (alist-get 'org-glance-overview-mode (buffer-local-variables))) (org-glance:log-warning "File \"%s\" is in `org-glance-overview' mode" file) nil)
                (t (list file)))))
    (cl-loop
     for file in files
     when (member (file-name-extension file) org-glance-scope:extensions)
     collect file)))

(cl-defmethod org-glance-scope ((l sequence))
  "Convert L to flattened list of files."
  (-some->> l
    (-keep #'org-glance-scope)
    -flatten
    seq-uniq))

(cl-defmethod org-glance-scope ((s symbol))
  "Return extracted S from `org-glance-scope--default-scope-alist'."
  (if-let (reserved-scope (assoc s org-glance-scope--default-scope-alist))
      (funcall (cdr reserved-scope))
    (org-glance-scope (symbol-name s))))

(cl-defmethod org-glance-scope ((b buffer))
  "Return list of files from buffer B."
  (list (condition-case nil (get-file-buffer b) (error b))))

(cl-defmethod org-glance-scope ((f function))
  "Adapt result of F."
  (-some->> f funcall org-glance-scope))

(defun org-glance-scope--choose-headline (choice headlines)
  "Deprecated helper."
  (--first (string= (org-glance-headline:title it) choice) headlines))

(cl-defun org-glance-scope-headlines (scope &optional (filter (lambda (headline) headline)))
  (cl-loop
     for file in (org-glance-scope scope)
     append (-non-nil (mapcar filter (org-glance-headline:extract-from file)))))

(org-glance:provide)
