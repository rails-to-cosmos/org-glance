(require 'org)

(eval-when-compile
  (require 'cl))

(eval-and-compile
  (require 'load-relative))

(defvar org-glance-scope--default-scope-alist
  `((file-with-archives . org-glance-scope--list-archives)
    (agenda . org-agenda-files)
    (agenda-with-archives . org-glance-scope--agenda-with-archives)))

(defconst org-glance-loc--glance-dir "GLANCE_DIR")

(defun org-glance-loc--get-rel-dirs (filename)
  "Get related directories from FILENAME.

Header property is specified in `org-glance-loc--glance-dir'.

For example, these lines in your agenda file mean that org-glance
will search views in \"archives\", \"resources\" and
\"~/docs/concerts\" (1) directories recursively:

#+GLANCE_DIR: ../resources
#+GLANCE_DIR: ./archives

* Concerto  (1)
  :PROPERTIES:
  :DIR:  ~/docs/concerts
  :END:

(1) (not implemented yet)"

  (let* ((default-directory (file-name-directory filename))
         (glance-dir-property (format "#+%s:" org-glance-loc--glance-dir)))
    (with-temp-buffer
      (insert-file-contents filename)
      (goto-char (point-min))
      (cl-loop while (search-forward glance-dir-property nil t)
               with glance-archive-dir
               do (setq glance-archive-dir
                        (-some->> (thing-at-point 'line)
                          substring-no-properties
                          (s-replace glance-dir-property "")
                          s-trim
                          file-truename))
               if (file-exists-p glance-archive-dir)
               collect glance-archive-dir into result
               else
               do (warn "glance-archive-dir from %s not found: %s" filename glance-archive-dir)
               finally (return (or result (list default-directory)))))))

(defun org-glance-loc--list-archives (filename)
  "Return list of org-archives for FILENAME.

Search glance directories recursively considering related
directories from `org-glance-loc--get-rel-dirs'."

  (let* ((archive-dirs (org-glance-loc--get-rel-dirs filename))
         (base-filename (-some->> filename
                          file-name-nondirectory
                          file-name-sans-extension))
         (archive-filename (-some->> base-filename
                             (s-append ".org_archive")))
         (org-filename (-some->> base-filename
                         (s-append ".org"))))
    (loop for archive-dir in archive-dirs
          append (directory-files-recursively archive-dir archive-filename)
          append (directory-files-recursively archive-dir org-filename))))

(defun org-glance-scope--list-archives ()
  (append (list (buffer-file-name))
          (org-glance-loc--list-archives (buffer-file-name))))

(defun org-glance-scope--agenda-with-archives ()
  (cl-loop for filename in (org-agenda-files)
           append (list filename)
           append (org-glance-loc--list-archives filename)))

(cl-defgeneric org-glance-scope--adapt (lfob)
  "Adapt list-file-or-buffer to list of file-or-buffers.")

(cl-defmethod org-glance-scope--adapt ((lfob string))
  "Return list of file LFOB if exists."
  (list (or (expand-file-name lfob)
            (-some->> lfob
              expand-file-name
              get-file-buffer
              buffer-name))))

(cl-defmethod org-glance-scope--adapt ((lfob sequence))
  "Adapt each element of LFOB."
  (-some->> lfob
    (-keep #'(lambda (fob) (->> fob org-glance-scope--adapt)))
    (-flatten)
    (seq-uniq)))

(cl-defmethod org-glance-scope--adapt ((lfob symbol))
  "Return extracted LFOB from `org-glance-scope--default-scope-alist'."
  (funcall (cdr (assoc lfob org-glance-scope--default-scope-alist))))

(cl-defmethod org-glance-scope--adapt ((lfob buffer))
  "Return list of LFOB."
  (list
   (condition-case nil
       (get-file-buffer lfob)
     (error lfob))))

(cl-defmethod org-glance-scope--adapt ((lfob function))
  "Adapt result of LFOB."
  (-some->> lfob
    funcall
    org-glance-scope--adapt))

(provide-me)
