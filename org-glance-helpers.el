;; -*- lexical-binding: t; -*-

(cl-defmacro org-glance-loop (&rest forms)
  "Loop over headlines and execute FORMS on each.
This is the anaphoric method, you can use `_' to call headline in forms."
  `(cl-loop for pos in (-non-nil
                        (org-element-map (org-element-parse-buffer 'headline) 'headline
                          (lambda (headline)
                            (when (= (org-element-property :level headline) 1)
                              (org-element-property :begin headline)))))
      collect (save-excursion
                (goto-char pos)
                (let ((<headline> (org-glance-headline-at-point)))
                  (org-glance:with-heading-at-point
                    ,@forms)))))

(cl-defmacro org-glance-with-file (file &rest forms)
  (declare (indent 1))
  `(with-temp-file ,file
     (org-mode)
     ,@forms))

(cl-defmacro org-glance-loop-file (file &rest forms)
  (declare (indent 1))
  `(org-glance-with-file ,file
     (insert-file-contents-literally ,file)
     (org-glance-loop
      (setf (org-glance-headline-origin <headline>) ,file)
      ,@forms)))

(cl-defmacro org-glance-loop-file-ro (file &rest forms)
  (declare (indent 1))
  `(with-temp-buffer
     (org-mode)
     (insert-file-contents-literally ,file)
     (org-glance-loop
      (setf (org-glance-headline-origin <headline>) ,file)
      ,@forms)))

(cl-defmacro org-glance-file-contents (file)
  "Return list of FILE contents. CAR of the list is string before
the first heading, CDR is a list of `org-glance-headlines'."
  (declare (indent 1))
  `(with-temp-buffer
     (org-mode)
     (insert-file-contents-literally ,file)
     (append
      (list (buffer-substring-no-properties (point-min) (save-excursion
                                                          (goto-char (point-min))
                                                          (unless (org-at-heading-p)
                                                            (outline-next-heading))
                                                          (point))))
      (org-glance-loop
       (setf (org-glance-headline-origin <headline>) ,file)
       <headline>))))

(cl-defmacro org-glance-loop-file-1 (file &rest forms)
  (declare (indent 1))
  `(car (-non-nil (org-glance-loop-file ,file ,@forms))))

(provide 'org-glance-helpers)
