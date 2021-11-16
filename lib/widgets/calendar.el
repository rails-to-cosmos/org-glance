(with-current-buffer (get-buffer-create "*glance-calendar*")
  (org-mode)
  (delete-region (point-min) (point-max))
  (insert (cl-loop
             with year = (caddr (calendar-current-date))
             for month in '(2 5 8 11)
             concat (with-temp-buffer
                      (calendar-generate month year)
                      (concat (buffer-string) "\n\n")))))
