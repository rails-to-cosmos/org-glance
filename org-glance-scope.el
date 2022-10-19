;; -*- lexical-binding: t; -*-

(require 'org)
(require 'files)
(require 'org-glance-types)

(cl-deftype org-glance-scope ()
  '(satisfies org-glance-scope-p))

(cl-defun org-glance-scope-p (scope)
  (dolist (file scope)
    (cl-check-type file org-glance-file)))

(defvar org-glance-scope-extensions
  '("org" "org_archive"))

(defvar org-glance-scope:default-scope-alist
  '((file-with-archives . org-glance-scope:file-with-archives)
    (agenda . org-agenda-files)
    (agenda-with-archives . -org-glance:agenda-with-archives)))

(defun org-glance-scope:file-with-archives ()
  (append (list (buffer-file-name))
          (org-glance-scope:list-file-archives (buffer-file-name))))

(defun org-glance-scope:list-file-archives (filename)
  "Return list of org-mode files for FILENAME."
  (let* ((dir (file-name-directory filename))
         (base-filename (-some->> filename
                          file-name-nondirectory
                          file-name-sans-extension)))
    (directory-files-recursively dir (format "%s.org\\.*" base-filename))))

(defun org-glance-scope:agenda-with-archives ()
  (cl-loop for filename in (org-agenda-files)
     append (list filename)
     append (org-glance-scope:list-file-archives filename)))

(cl-defgeneric org-glance-scope (origin)
  "Convert ORIGIN to list of files.")

(cl-defmethod org-glance-scope ((file string))
  "Return list of file S if exists."
  (let ((files (cond
                 ((not (file-exists-p file)) (warn "File \"%s\" does not exist" file) nil)
                 ((not (file-readable-p file)) (warn "File \"%s\" is not readable" file) nil)
                 ((f-directory? file) (org-glance-scope (directory-files-recursively file "\\.*.org\\.*")))
                 ;; Filter files in special modes: `org-glance-material' and `org-glance-overview' files.
                 ;; ((with-temp-buffer
                 ;;    (insert-file-contents file)
                 ;;    (hack-local-variables)
                 ;;    (alist-get 'org-glance-overview-mode (buffer-local-variables))) (warn "File \"%s\" is in `org-glance-overview' mode" file) nil)
                 (t (list file)))))
    (cl-loop for file in files
       when (and (file-equal-p file (file-name-sans-versions file))
                 (member (file-name-extension file) org-glance-scope-extensions))
       collect file)))

(cl-defmethod org-glance-scope ((l sequence))
  "Convert L to flattened list of files."
  (-some->> l
    (-keep #'org-glance-scope)
    -flatten
    seq-uniq))

(cl-defmethod org-glance-scope ((s symbol))
  "Return extracted S from `org-glance-scope:default-scope-alist'."
  (if-let (reserved-scope (assoc s org-glance-scope:default-scope-alist))
      (funcall (cdr reserved-scope))
    (org-glance-scope (symbol-name s))))

(cl-defmethod org-glance-scope ((b buffer))
  "Return list of files from buffer B."
  (list (condition-case nil (get-file-buffer b) (error b))))

(cl-defmethod org-glance-scope ((f function))
  "Adapt result of F."
  (-some->> f funcall org-glance-scope))

(provide 'org-glance-scope)
