(require 'load-relative)

(defvar org-glance-org-scope-extensions
  '("org" "org_archive"))

(cl-defgeneric org-glance-scope (lfob)
  "Adapt list-file-or-buffer to list of files.")

(cl-defmethod org-glance-scope ((lfob string))
  "Return list of file LFOB if exists."
  (let ((file (expand-file-name lfob)))
    (cond
      ((not (file-exists-p file)) (warn "File %s does not exist" file) nil)
      ((not (file-readable-p file)) (warn "File %s is not readable" file) nil)
      ((f-directory? file) (org-glance-list-files-recursively file))
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

(defvar org-glance-scope--default-scope-alist
  '((file-with-archives . -org-glance-list-archives)
    (agenda . org-agenda-files)
    (agenda-with-archives . -org-glance-agenda-with-archives)))

(defcustom org-glance-default-scope '(agenda-with-archives)
  "Default scope for glancing views."
  :group 'org-glance
  :type 'list)

(defun org-glance-prompt-headlines (prompt headlines)
  (org-completing-read prompt (mapcar #'org-glance-format headlines)))

(defun org-glance-choose-headline (choice headlines)
  (--first (string= (org-glance-format it) choice) headlines))

(defmacro org-glance-with-headline-narrowed (headline &rest forms)
  "Visit HEADLINE, narrow to its subtree and execute FORMS on it."
  (declare (indent defun))
  `(let* ((file (org-element-property :file ,headline))
          (file-buffer (get-file-buffer file))
          (visited-buffer (current-buffer)))
     (org-glance-action-call 'visit :on ,headline)
     (widen)
     (org-narrow-to-subtree)
     (unwind-protect
          (let ((org-link-frame-setup (cl-acons 'file 'find-file org-link-frame-setup)))
            ,@forms)
       (widen))
     (cond ((and file-buffer (not (eq file-buffer (current-buffer)))) (bury-buffer file-buffer))
           ((and file-buffer (eq file-buffer (current-buffer))) (progn (switch-to-buffer visited-buffer)
                                                                       (bury-buffer file-buffer)))
           (t (kill-buffer (get-file-buffer file))))))

(cl-defmacro org-glance-with-headline-materialized (headline &rest forms)
  "Materialize HEADLINE, execute FORMS in materialized buffer."
  (declare (indent defun))
  `(let* ((file (org-element-property :file ,headline))
          (file-buffer (get-file-buffer file)))
     (org-glance-action-call 'materialize :on ,headline)
     (unwind-protect
          (let ((org-link-frame-setup (cl-acons 'file 'find-file org-link-frame-setup)))
            ,@forms)
       (kill-buffer org-glance-materialized-view-buffer))
     (cond (file-buffer (bury-buffer file-buffer))
           (t (kill-buffer (get-file-buffer file))))))

(defun org-glance-scope-headlines (scope &optional filter)
  (cl-loop
     for file in (org-glance-scope scope)
     when (member (file-name-extension file) org-glance-org-scope-extensions)
     do (message "Run org-glance on headlines in file %s" file)
     append (org-glance-read-headlines-from-file file filter)
     into result
     do (redisplay)
     finally (cl-return result)))

(provide-me)
