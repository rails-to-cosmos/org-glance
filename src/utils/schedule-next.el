(cl-defun org-glance-schedule-next (date &optional (time "") (modifier ""))
  (let ((time-format "%Y-%m-%d %a %H:%M"))
    (concat "<"
            (format-time-string time-format (org-read-date
                                             (not (string-empty-p time))
                                             'to-time
                                             (concat date " " time)
                                             nil))
            (if (string-empty-p modifier) "" (concat " " modifier))
            ">")))

(provide-me)
