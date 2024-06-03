(require 'org-glance-module)

(org-glance:require org)

(defvar-local -org-tss:local-timestamps '())

(define-minor-mode org-tss-mode "Handle multiple repeatable timestamps."
  nil nil nil
  (cond
    (org-tss-mode (advice-add 'org-auto-repeat-maybe :before #'org-tss-capture)
                  (advice-add 'org-auto-repeat-maybe :after #'org-tss-restore))
    (t (advice-remove 'org-auto-repeat-maybe #'org-tss-capture)
       (advice-remove 'org-auto-repeat-maybe #'org-tss-restore))))

(cl-defun org-tss-sort-timestamps (tss)
  "Sort TSS."
  (sort tss #'(lambda (lhs rhs) (time-less-p
                            (org-time-string-to-time (org-element-property :raw-value lhs))
                            (org-time-string-to-time (org-element-property :raw-value rhs))))))

(cl-defun org-tss-buffer-timestamps (&optional (org-data (org-element-parse-buffer)))
  (org-element-map org-data '(timestamp) #'identity))

(cl-defun org-tss-buffer-schedules (&optional (org-data (org-element-parse-buffer)))
  (org-element-map org-data '(headline) #'(lambda (headline) (org-element-property :scheduled headline))))

(cl-defun org-tss-buffer-deadlines (&optional (org-data (org-element-parse-buffer)))
  (org-element-map org-data '(headline) #'(lambda (headline) (org-element-property :deadline headline))))

(cl-defun org-tss-headline-timestamps (&rest includes)
  (save-restriction
    (org-narrow-to-subtree)
    (cl-loop for timestamp in (let ((org-data (org-element-parse-buffer)))
                                (append (org-tss-buffer-timestamps org-data)
                                        (when (member 'include-schedules includes)
                                          (org-tss-buffer-schedules org-data))
                                        (when (member 'include-deadlines includes)
                                          (org-tss-buffer-deadlines org-data))))
       collect timestamp)))

(cl-defun org-tss-filter-active (tss)
  (--filter (member (org-element-property :type it) '(active active-range)) tss))

(cl-defun org-tss-filter-repeated (tss)
  (--filter (and (member (org-element-property :type it) '(active active-range))
                 (> (or (org-element-property :repeater-value it) 0) 0))
            tss))

(cl-defun org-tss-capture (&rest args)
  (setq-local -org-tss:local-timestamps (-some->> (org-tss-headline-timestamps)
                                          (org-tss-filter-active)
                                          (org-tss-filter-repeated)
                                          (org-tss-sort-timestamps))))

(cl-defun org-tss-restore (&rest args)
  (let ((standard-output 'ignore)
        (tss* (-some->> (org-tss-headline-timestamps)
                (org-tss-filter-active)
                (org-tss-filter-repeated)
                (org-tss-sort-timestamps))))
    (cl-loop
       for tsi from 1 below (length tss*)
       for ts = (nth tsi -org-tss:local-timestamps)
       for ts* = (nth tsi tss*)
       do (save-excursion
            (goto-char (org-element-property :begin ts*))
            (delete-region (org-element-property :begin ts*)
                           (org-element-property :end ts*))
            (insert (org-element-property :raw-value ts))))))

(cl-defun org-tss-reset-buffer-timestamps-except-earliest ()
  "Reset active timestamps in buffer except earliest."
  (let ((standard-output 'ignore)
        (tss (-some->> (org-tss-headline-timestamps 'include-schedules 'include-deadlines)
               (org-tss-filter-active)
               (org-tss-filter-repeated)
               (org-tss-sort-timestamps))))
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

(cl-defun org-tss-headline-repeated-p ()
  "Is headline at point repeated?"
  (save-excursion
    (unless (org-at-heading-p)
      (org-back-to-heading-or-point-min))
    (when (append
           (-some->> (org-tss-headline-timestamps 'include-schedules 'include-deadlines)
             (org-tss-filter-active)
             (org-tss-filter-repeated)
             (org-tss-sort-timestamps)))
      t)))

(org-glance:provide)
