(require 'load-relative)
(require 'org-glance-module)
(require 'dash)

(org-glance-module-import lib.utils.helpers)
(org-glance-module-import lib.core.headline)

(declare-function org-glance--list-files-recursively "lib/utils/helpers.el")
(declare-function org-glance-headline:scan-file "lib/utils/helpers.el")

(require 'org)

(defvar org-glance-scope:extensions
  '("org" "org_archive"))

(defvar org-glance-scope--default-scope-alist
  '((file-with-archives . org-glance--list-archives)
    (agenda . org-agenda-files)
    (agenda-with-archives . org-glance--agenda-with-archives)))

(cl-defgeneric org-glance-scope (lfob)
  "Adapt list-file-or-buffer to list of files.")

(cl-defmethod org-glance-scope ((lfob string))
  "Return list of file LFOB if exists."
  (let* ((file (expand-file-name lfob))
         (files (cond
                  ((not (file-exists-p file)) (warn "File %s does not exist" file) nil)
                  ((not (file-readable-p file)) (warn "File %s is not readable" file) nil)
                  ((f-directory? file) (org-glance--list-files-recursively file))
                  (t (list file)))))
    (cl-loop for file in files
       when (member (file-name-extension file) org-glance-scope:extensions)
       collect file)))

(cl-defmethod org-glance-scope ((lfob sequence))
  "Adapt each element of LFOB."
  (-some->> lfob
    (-keep #'org-glance-scope)
    -flatten
    seq-uniq))

(cl-defmethod org-glance-scope ((lfob symbol))
  "Return extracted LFOB from `org-glance-scope--default-scope-alist'."
  (funcall (cdr (assoc lfob org-glance-scope--default-scope-alist))))

(cl-defmethod org-glance-scope ((lfob buffer))
  "Return list of files from LFOB buffer."
  (list
   (condition-case nil
       (get-file-buffer lfob)
     (error lfob))))

(cl-defmethod org-glance-scope ((lfob function))
  "Adapt result of LFOB."
  (-some->> lfob
    funcall
    org-glance-scope))

(defcustom org-glance-default-scope '(agenda-with-archives)
  "Default scope for glancing views."
  :group 'org-glance
  :type 'list)

(defun org-glance-scope--prompt-headlines (prompt headlines)
  (org-completing-read prompt (mapcar #'org-glance-headline:title headlines)))

(defun org-glance-scope--choose-headline (choice headlines)
  (--first (string= (org-glance-headline:title it) choice) headlines))

(cl-defun org-glance-scope-headlines (scope &optional (filter (lambda (headline) headline)))
  (cl-loop for file in (org-glance-scope scope)
     for headlines = (org-glance-headline:scan-file file)
     append (-non-nil (mapcar filter headlines))))

(org-glance-module-provide)
