(require 'org-glance-module)

(defconst org-glance-agenda:header "#    -*- mode: org; mode: org-glance-agenda -*-")

(defvar org-glance-agenda-mode-map (make-sparse-keymap)
  "Manipulate `org-mode' entries in `org-glance-agenda-mode'.")

(cl-defun org-glance-agenda-location ()
  (f-join org-glance-directory "glance.org"))

(define-minor-mode org-glance-agenda-mode
    "A minor read-only mode to use in glance agenda files."
  nil nil org-glance-agenda-mode-map
  (read-only-mode 'toggle))

(cl-defun org-glance-overview:calendar-widget (&optional (date (calendar-current-date)))
  (with-temp-buffer

    (insert
     (with-temp-buffer
       (calendar-generate-month (car date) (caddr date) 0)
       (buffer-substring-no-properties (point-min) (point-max))))

    (goto-char (point-min))

    ;; (while (re-search-forward "\\([[:digit:]]\\{4\\}\\)" nil t)
    ;;   (replace-match "[[elisp:(-og-calw-y \\1)][\\1]]"))

    ;; (while (re-search-forward "\\([[:digit:]]\\{1,2\\}\\)" nil t)
    ;;   (if (= (string-to-number (match-string 1)) (cadr date))
    ;;       (replace-match "*\\1*")
    ;;     (replace-match "[[elisp:(-og-calw-d \\1)][\\1]]")))

    (buffer-substring-no-properties (point-min) (point-max))))

(defun org-glance-agenda-display-entry (entry)
  "Function to display a specific (org) entry"

  (let* ((text        (get-text-property 0 'txt entry))
         (time        (get-text-property 0 'time entry))
         (time-of-day (get-text-property 0 'time-of-day entry))
         (hours       (if time-of-day
                          (format "/%02dh —/" (floor (/ time-of-day 100)))
                        "     "))
         (minutes     (if time-of-day
                          (% time-of-day 100) -1))
         (duration    (get-text-property 0 'duration entry)))
    ;; (insert (format "%s %s\n" hours text))
    (insert (format "%s \t %s\n" time text))))

(cl-defun org-glance-agenda ()
  (interactive)
  (with-temp-file (org-glance-agenda-location)
    (let ((inhibit-read-only t)
          (calendar (org-glance-overview:calendar-widget))
          (date (list 12 3 2021))
          (entries '()))
      (insert org-glance-agenda:header
              "\n\n"
              calendar
              "\n\n")

      (dolist (file (org-agenda-files))
        (dolist (entry (org-agenda-get-day-entries file date))
          (if (get-text-property 0 'time-of-day entry)
              (add-to-list 'entries entry))))

      (setq entries (sort entries #'(lambda (entry-1 entry-2)
                                      (<
                                       (get-text-property 0 'time-of-day entry-1)
                                       (get-text-property 0 'time-of-day entry-2)))))

      (dolist (entry entries)
        (pp (get-text-property 0 'org-marker entry) ;; (text-properties-at 0 entry)
            )
        (org-glance-agenda-display-entry entry))))
  (find-file (org-glance-agenda-location)))

(cl-defun org-glance-agenda:daily (class date)
  (let ((entries (org-agenda-get-day-entries
                  (org-glance-overview:location class)
                  date)))
    (cl-loop
       for entry in entries
       for marker = (get-text-property 0 'org-marker entry)
       collect (save-window-excursion
                 (org-goto-marker-or-bmk marker)
                 (->> (org-glance-headline:at-point)
                      org-glance-headline:id
                      org-glance-metastore:get-headline
                      org-glance-headline:visit)
                 (org-glance-headline:at-point)))))

;; (cl-loop
;;    for headline in (org-glance-agenda:daily 'task (list 12 20 2021))
;;    collect (org-glance-headline:with-materialized-headline headline
;;              (cons (org-glance-headline:title headline) (length (org-glance-headline:subtasks)))))

;; 0. Filter tasks!
;; 1. Unfinished tasks count.

(org-glance:provide)
