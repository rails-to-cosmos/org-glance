(cl-defgeneric org-glance-adapt-scope (lfob)
  "Adapt list-file-or-buffer to list of file-or-buffers.")

(cl-defmethod org-glance-adapt-scope ((lfob string))
  "Return list of file LFOB if exists."
  (list (or (expand-file-name lfob)
            (-some->> lfob
                      expand-file-name
                      get-file-buffer
                      buffer-name))))

(cl-defmethod org-glance-adapt-scope ((lfob sequence))
  "Adapt each element of LFOB."
  (-some->> lfob
            (-keep #'(lambda (fob) (->> fob org-glance-adapt-scope)))
            (-flatten)
            (seq-uniq)))

(cl-defmethod org-glance-adapt-scope ((lfob symbol))
  "Return extracted LFOB from `org-glance--default-scopes-alist'."
  (-some->> lfob
            (funcall (-cut alist-get <> org-glance--default-scopes-alist))
            (funcall)))

(cl-defmethod org-glance-adapt-scope ((lfob buffer))
  "Return list of LFOB."
  (list
   (condition-case nil
       (get-file-buffer lfob)
     (error lfob))))

(cl-defmethod org-glance-adapt-scope ((lfob function))
  "Adapt result of LFOB."
  (-some->> lfob
            funcall
            org-glance-adapt-scope))

(provide 'org-glance-adapter)
;;; org-glance-adapter.el ends here
