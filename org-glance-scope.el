(eval-when-compile
  (require 'cl))

(defvar org-glance-scope--default-scope-alist
  `((file-with-archives . org-glance-scope--list-archives)
    (agenda . org-agenda-files)
    (agenda-with-archives . org-glance-scope--agenda-with-archives)))

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

(provide 'org-glance-scope)
