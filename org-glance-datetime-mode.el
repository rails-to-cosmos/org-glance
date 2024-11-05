(defvar-local -org-glance-datetime:local-timestamps '())

(define-minor-mode org-glance-datetime-mode "Handle multiple repeatable timestamps."
  :lighter nil
  :global nil
  :group 'glance
  (cond (org-glance-datetime-mode (advice-add 'org-auto-repeat-maybe :before #'org-glance-datetime-capture)
                            (advice-add 'org-auto-repeat-maybe :after #'org-glance-datetime-restore))
        (t (advice-remove 'org-auto-repeat-maybe #'org-glance-datetime-capture)
           (advice-remove 'org-auto-repeat-maybe #'org-glance-datetime-restore))))

(cl-defun org-glance-datetime-sort-timestamps (tss)
  "Sort TSS."
  (sort tss #'(lambda (lhs rhs) (time-less-p
                            (org-time-string-to-time (org-element-property :raw-value lhs))
                            (org-time-string-to-time (org-element-property :raw-value rhs))))))

(cl-defun org-glance-datetime-buffer-timestamps (&optional (org-data (org-element-parse-buffer)))
  (org-element-map org-data '(timestamp) #'identity))

(cl-defun org-glance-datetime-buffer-schedules (&optional (org-data (org-element-parse-buffer)))
  (org-element-map org-data '(headline) #'(lambda (headline) (org-element-property :scheduled headline))))

(cl-defun org-glance-datetime-buffer-deadlines (&optional (org-data (org-element-parse-buffer)))
  (org-element-map org-data '(headline) #'(lambda (headline) (org-element-property :deadline headline))))

(cl-defun org-glance-datetime-headline-timestamps (&rest includes)
  (save-restriction
    (org-narrow-to-subtree)
    (cl-loop for timestamp in (let ((org-data (org-element-parse-buffer)))
                                (append (org-glance-datetime-buffer-timestamps org-data)
                                        (when (member 'include-schedules includes)
                                          (org-glance-datetime-buffer-schedules org-data))
                                        (when (member 'include-deadlines includes)
                                          (org-glance-datetime-buffer-deadlines org-data))))
       collect timestamp)))

(cl-defun org-glance-datetime-filter-active (tss)
  (--filter (member (org-element-property :type it) '(active active-range)) tss))

(cl-defun org-glance-datetime-filter-repeated (tss)
  (--filter (and (member (org-element-property :type it) '(active active-range))
                 (> (or (org-element-property :repeater-value it) 0) 0))
            tss))

(cl-defun org-glance-datetime-capture (&rest args)
  (setq-local -org-glance-datetime:local-timestamps (-some->> (org-glance-datetime-headline-timestamps)
                                          (org-glance-datetime-filter-active)
                                          (org-glance-datetime-filter-repeated)
                                          (org-glance-datetime-sort-timestamps))))

(cl-defun org-glance-datetime-restore (&rest args)
  (let ((standard-output 'ignore)
        (tss* (-some->> (org-glance-datetime-headline-timestamps)
                (org-glance-datetime-filter-active)
                (org-glance-datetime-filter-repeated)
                (org-glance-datetime-sort-timestamps))))
    (cl-loop
       for tsi from 1 below (length tss*)
       for ts = (nth tsi -org-glance-datetime:local-timestamps)
       for ts* = (nth tsi tss*)
       do (save-excursion
            (goto-char (org-element-property :begin ts*))
            (delete-region (org-element-property :begin ts*)
                           (org-element-property :end ts*))
            (insert (org-element-property :raw-value ts))))))

(cl-defun org-glance-datetime-reset-buffer-timestamps-except-earliest ()
  "Reset active timestamps in buffer except earliest."
  (let ((standard-output 'ignore)
        (tss (-some->> (org-glance-datetime-headline-timestamps 'include-schedules 'include-deadlines)
               (org-glance-datetime-filter-active)
               (org-glance-datetime-filter-repeated)
               (org-glance-datetime-sort-timestamps))))
    (cl-loop
       for ts in tss
       for index from 0
       do
         (goto-char (org-element-property :begin ts))
         (if (> index 0)
             (org-toggle-timestamp-type)
           ;; reset repeater
           (save-excursion
             (let ((bound1 (org-element-property :begin ts))
                   (bound0 (org-element-property :end ts)))
               (when (and (re-search-forward
                           (concat "\\(" org-scheduled-time-regexp "\\)\\|\\("
                                   org-deadline-time-regexp "\\)\\|\\("
                                   org-ts-regexp "\\)")
                           bound0 t)
                          (re-search-backward "[ \t]+\\(?:[.+]\\)?\\+\\([0-9]+\\)[hdwmy]"
                                              bound1 t))
                 (replace-match "0" t nil nil 1))))))))

(cl-defun org-glance-datetime-headline-repeated-p ()
  "Is headline at point repeated?"
  (save-excursion
    (unless (org-at-heading-p)
      (org-back-to-heading-or-point-min))
    (when (append
           (-some->> (org-glance-datetime-headline-timestamps 'include-schedules 'include-deadlines)
             (org-glance-datetime-filter-active)
             (org-glance-datetime-filter-repeated)
             (org-glance-datetime-sort-timestamps)))
      t)))

(provide 'org-glance-datetime-mode)
