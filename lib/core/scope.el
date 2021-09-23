(require 'load-relative)
(require 'org-glance-module)
(require 'dash)

(org-glance:require lib.utils.helpers)
(org-glance:require lib.core.headline)

(declare-function -org-glance:list-files-recursively "lib/utils/helpers.el")
(declare-function org-glance-headline:scan-file "lib/utils/helpers.el")

(require 'org)

(defvar org-glance-scope:extensions
  '("org" "org_archive"))

(defvar org-glance-scope--default-scope-alist
  '((file-with-archives . -org-glance:file-with-archives)
    (agenda . org-agenda-files)
    (agenda-with-archives . -org-glance:agenda-with-archives)))

(cl-defgeneric org-glance-scope (_)
  "Convert input to list of files if possible.")

(cl-defmethod org-glance-scope ((s string))
  "Return list of file S if exists."
  (let ((file (expand-file-name s)))
    (cl-loop for file in (cond
                           ((not (file-exists-p file)) (warn "File %s does not exist" file) nil)
                           ((not (file-readable-p file)) (warn "File %s is not readable" file) nil)
                           ((f-directory? file) (-org-glance:list-files-recursively file))
                           (t (list file)))
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
  (cl-loop for file in (org-glance-scope scope)
     append (-non-nil (mapcar filter
                              (with-temp-buffer
                                (message "Scan file %s" file)
                                (redisplay)
                                (insert-file-contents file)
                                (hack-local-variables)
                                (unless (alist-get 'org-glance-overview-mode (buffer-local-variables))
                                  (org-mode)
                                  (org-glance-headline:scan-buffer (current-buffer) :file (abbreviate-file-name file))))))))

(org-glance:provide)
