(require 'load-relative)
(require 'org-glance-module)

(org-glance-module-import lib.utils.helpers)

(declare-function org-glance--list-files-recursively "lib/utils/helpers.el")
(declare-function org-glance--read-headlines-from-file "lib/utils/helpers.el")
(declare-function org-glance--format-headline "lib/utils/helpers.el")

(require 'org)

(defvar org-glance-org-scope-extensions
  '("org" "org_archive"))

(defvar org-glance-scope--default-scope-alist
  '((file-with-archives . org-glance--list-archives)
    (agenda . org-agenda-files)
    (agenda-with-archives . org-glance--agenda-with-archives)))

(cl-defgeneric org-glance-scope (lfob)
  "Adapt list-file-or-buffer to list of files.")

(cl-defmethod org-glance-scope ((lfob string))
  "Return list of file LFOB if exists."
  (let ((file (expand-file-name lfob)))
    (cond
      ((not (file-exists-p file)) (warn "File %s does not exist" file) nil)
      ((not (file-readable-p file)) (warn "File %s is not readable" file) nil)
      ((f-directory? file) (org-glance--list-files-recursively file))
      (t file))))

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

(defun org-glance-prompt-headlines (prompt headlines)
  (org-completing-read prompt (mapcar #'org-glance--format-headline headlines)))

(defun org-glance-choose-headline (choice headlines)
  (--first (string= (org-glance--format-headline it) choice) headlines))

(defun org-glance-scope-headlines (scope &optional filter)
  (cl-loop
     for file in (org-glance-scope scope)
     when (member (file-name-extension file) org-glance-org-scope-extensions)
     do (message "Glance file %s" file)
     append (org-glance--read-headlines-from-file file filter)
     into result
     do (redisplay)
     finally (cl-return result)))

(provide-me)
