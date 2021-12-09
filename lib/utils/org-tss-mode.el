(require 'org-glance-module)

(org-glance:require org)

(defvar-local -org-tss:local-timestamps '())

(define-minor-mode org-tss-mode "Handle multiple repeatable timestamps."
  nil nil nil
  (cond
    (org-tss-mode (advice-add 'org-auto-repeat-maybe :before #'org-tss:capture)
                  (advice-add 'org-auto-repeat-maybe :after #'org-tss:restore))
    (t (advice-remove 'org-auto-repeat-maybe #'org-tss:capture)
       (advice-remove 'org-auto-repeat-maybe #'org-tss:restore))))

(cl-defun org-tss:get-buffer-timestamps ()
  (sort (save-restriction
          (org-narrow-to-subtree)
          (cl-loop
             for timestamp in (org-element-map (org-element-parse-buffer) '(timestamp) #'identity)
             when (and (member (org-element-property :type timestamp) '(active active-range))
                       (> (or (org-element-property :repeater-value timestamp) 0) 0))
             collect timestamp))
        #'(lambda (lhs rhs) (time-less-p
                        (org-time-string-to-time (org-element-property :raw-value lhs))
                        (org-time-string-to-time (org-element-property :raw-value rhs))))))

(cl-defun org-tss:capture (&rest args)
  (setq-local -org-tss:local-timestamps (org-tss:get-buffer-timestamps)))

(cl-defun org-tss:restore (&rest args)
  (let ((-org-tss:local-timestamps* (org-tss:get-buffer-timestamps)))
    (cl-loop
       for tsi from 1 below (length -org-tss:local-timestamps*)
       for ts = (nth tsi -org-tss:local-timestamps)
       for ts* = (nth tsi -org-tss:local-timestamps*)
       do (save-excursion
            (goto-char (org-element-property :begin ts*))
            (delete-region (org-element-property :begin ts*)
                           (org-element-property :end ts*))
            (insert (org-element-property :raw-value ts))))))

(cl-defun org-tss:reset-buffer-timestamps-except-earliest ()
  "Reset active timestamps in buffer except earliest."
  (let ((tss (org-tss:get-buffer-timestamps)))
    (progn
      (cl-loop
         for ts in tss
         for index from 0
         do
           (goto-char (org-element-property :begin ts))
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
	         (replace-match "0" t nil nil 1))))

           (when (> index 0)
             (org-toggle-timestamp-type))))))

(org-glance:provide)
