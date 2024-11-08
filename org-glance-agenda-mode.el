(defconst org-glance-agenda:header "#    -*- mode: org; mode: org-glance-agenda -*-")

(defvar org-glance-agenda-mode-map (make-sparse-keymap)
  "Manipulate `org-mode' entries in `org-glance-agenda-mode'.")

(define-key org-glance-agenda-mode-map (kbd "n") #'next-line)
(define-key org-glance-agenda-mode-map (kbd "p") #'previous-line)
(define-key org-glance-agenda-mode-map (kbd "q") #'bury-buffer)
(define-key org-glance-agenda-mode-map (kbd "g") #'org-glance-agenda)
(define-key org-glance-agenda-mode-map (kbd "RET") #'org-open-at-point)

(cl-defun org-glance-date-format (date)
  (seq-let (month day year) date
    (format "%d-%02d-%02d" year month day)))

(cl-defun org-glance-agenda-location (&optional (date (org-glance-agenda--read-date)))
  (-org-glance:make-file-directory
   (f-join org-glance-directory
           "agenda"
           (format "%s.org" (org-glance-date-format date)))))

(define-minor-mode org-glance-agenda-mode "A minor read-only mode to use in glance agenda files."
  :lighter nil
  :global nil
  :group 'glance
  :keymap org-glance-agenda-mode-map
  (read-only-mode 'toggle))

(cl-defun org-glance-overview:calendar-widget (&optional (date (org-glance-agenda--read-date)))
  (with-temp-buffer
    (save-excursion
      (calendar-generate-month (car date) (caddr date) 0))
    (replace-regexp (format "[[:space:]]\\(%d\\)[[:space:]]" (cadr date)) " _\\1_ ")
    (buffer-string)))


(cl-defun org-glance-agenda--read-date ()
  (let ((ds (ts-parse (org-read-date))))
    (list
     (ts-month ds)
     (ts-day ds)
     (ts-year ds))))

(cl-defun org-glance-agenda (&optional (date (org-glance-agenda--read-date)))
  (interactive)
  (let ((agenda-location (org-glance-agenda-location date)))

    (with-temp-file agenda-location
      (let ((entries (cl-loop
                        for file in (org-agenda-files)
                        when (file-exists-p file)
                        append (cl-loop for entry in (org-agenda-get-day-entries file date)
                                  ;; when (get-text-property 0 'time-of-day entry)
                                  collect entry)
                        into entries
                        finally (return (sort entries #'(lambda (entry-1 entry-2)
                                                          (<
                                                           (or (get-text-property 0 'time-of-day entry-1) 0)
                                                           (or (get-text-property 0 'time-of-day entry-2) 0))))))))

        (insert
         (s-join "\n\n"
                 (list
                  org-glance-agenda:header
                  (org-glance-overview:calendar-widget date)))
         "\n\n")

        (cl-loop
           with headlines = (make-hash-table)
           for entry in entries
           for marker = (get-text-property 0 'org-marker entry)
           for headline = (save-window-excursion
                            (org-goto-marker-or-bmk marker)
                            (org-glance:with-headline-narrowed (org-glance-overview:original-headline)
                              (list
                               :ref (org-glance-headline-reference)
                               :id (org-glance-headline:id))))
           for id = (plist-get headline :id)
           when (and (not (null id)) (not (gethash (intern id) headlines)))
           do (let* ((text        (get-text-property 0 'txt entry))
                     (time        (get-text-property 0 'time entry))
                     (time-of-day (get-text-property 0 'time-of-day entry))
                     (hours       (if time-of-day
                                      (format "/%02dh —/" (floor (/ time-of-day 100)))
                                    "     "))
                     (minutes     (if time-of-day
                                      (% time-of-day 100) -1))
                     (duration    (get-text-property 0 'duration entry)))
                (puthash (intern id) t headlines)
                (insert (format "%s \t %s\n" time
                                (plist-get headline :ref)))))))

    (when (buffer-live-p (get-file-buffer agenda-location))
      (kill-buffer (get-file-buffer agenda-location)))

    (find-file agenda-location)))

(cl-defun org-glance-agenda:daily (class date)
  (let ((entries (org-agenda-get-day-entries
                  (org-glance-overview:location class)
                  date)))
    (cl-loop
       for entry in entries
       for marker = (get-text-property 0 'org-marker entry)
       collect (org-glance:with-headline-narrowed
                   (->> (org-glance-headline:at-point)
                        org-glance-headline:id
                        org-glance-metastore:get-headline)
                 (org-glance-headline:at-point)))))

;; (cl-loop
;;    for headline in (org-glance-agenda:daily 'task (list 12 20 2021))
;;    collect (org-glance:with-headline-materialized headline
;;              (cons (org-glance-headline:title headline) (length (org-glance-headline:subtasks)))))

;; 0. Filter tasks!
;; 1. Unfinished tasks count.

;; (cl-loop
;;    for headline in (cl-loop
;;                       for file in (org-agenda-files)
;;                       when (file-exists-p file)
;;                       append (org-glance-headline:extract-from file))
;;    collect (org-glance:with-headline-narrowed headline
;;               (org-glance-headline:archived?)))

(provide 'org-glance-agenda-mode)
