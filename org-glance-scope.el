(require 'org)

(eval-when-compile
  (require 'cl))

(eval-and-compile
  (require 'load-relative))

(defvar org-glance-scope--default-scope-alist
  '((file-with-archives . -org-glance-list-archives)
    (agenda . org-agenda-files)
    (agenda-with-archives . -org-glance-agenda-with-archives)))

(defun -org-glance-list-archives ()
  (append (list (buffer-file-name))
          (org-glance-list-file-archives (buffer-file-name))))

(defun -org-glance-agenda-with-archives ()
  (cl-loop for filename in (org-agenda-files)
           append (list filename)
           append (org-glance-list-file-archives filename)))

(cl-defun org-glance-headlines
    (&key db
          (scope '(agenda))
          (filter #'(lambda (_) t))
          (db-init nil))
  (let* ((create-db? (or (and db db-init) (and db (not (file-exists-p db)))))
         (load-db? (and (not (null db)) (file-exists-p db)))
         (skip-db? (null db)))
    (cond (create-db? (org-glance-db-init db (org-glance-scope-headlines scope filter)))
          (load-db?   (org-glance-db-load db))
          (skip-db?   (org-glance-scope-headlines scope filter))
          (t         (user-error "Nothing to glance at (scope: %s)" scope)))))

(cl-defmethod org-glance-filter-apply (filter headline)
  (or (null filter) (and filter (funcall filter headline))))

(cl-defmethod org-glance-scope-headlines (scope &optional filter)
  (cl-loop
   for file in (org-glance-scope scope)
   do (message "Glance %s" file)
   append (org-glance-read-headlines-from-file file filter)
   into result
   do (redisplay)
   finally (cl-return result)))

(cl-defmethod org-glance-read-headlines-from-file ((file string) &optional filter)
  (with-temp-buffer
    (insert-file-contents file)
    (org-element-map (org-element-parse-buffer 'headline) 'headline
      (lambda (headline)
        (when (org-glance-filter-apply filter headline)
          (plist-put (cadr headline) :file file)
          headline)))))

(defun org-glance-list-files-recursively (dir)
  (directory-files-recursively dir "\\.*.org\\.*"))

(defun org-glance-list-file-archives (filename)
  "Return list of org-mode files for FILENAME."
  (let* ((dir (file-name-directory filename))
         (base-filename (-some->> filename
                          file-name-nondirectory
                          file-name-sans-extension)))
    (directory-files-recursively dir (format "%s.org\\.*" base-filename))))

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

(provide-me)
