;; -*- lexical-binding: t -*-

(require 'dash)
(require 'cl-lib)
(require 'org)
(require 'org-element)

(defvar-local -org-glance-datetime:local-timestamps '())

(define-minor-mode org-glance-datetime-mode "Handle multiple repeatable timestamps."
  :lighter nil
  :global nil
  :group 'glance
  (cond (org-glance-datetime-mode (advice-add 'org-auto-repeat-maybe :before #'org-glance-datetime-capture)
                                  (advice-add 'org-auto-repeat-maybe :after #'org-glance-datetime-restore))
        (t (advice-remove 'org-auto-repeat-maybe #'org-glance-datetime-capture)
           (advice-remove 'org-auto-repeat-maybe #'org-glance-datetime-restore))))

(cl-defun org-glance-datetime-active-repeated-timestamps (&rest includes)
  "Subtree's active repeated timestamps, sorted ascending.
INCLUDES may add `include-schedules' / `include-deadlines' planning
timestamps.  One `org-element' parse of the narrowed subtree."
  (save-restriction
    (org-narrow-to-subtree)
    (let* ((org-data (org-element-parse-buffer))
           (tss (append
                 (org-element-map org-data '(timestamp) #'identity)
                 (when (member 'include-schedules includes)
                   (org-element-map org-data '(headline)
                     (lambda (h) (org-element-property :scheduled h))))
                 (when (member 'include-deadlines includes)
                   (org-element-map org-data '(headline)
                     (lambda (h) (org-element-property :deadline h)))))))
      (sort (--filter (and (memq (org-element-property :type it) '(active active-range))
                           (> (or (org-element-property :repeater-value it) 0) 0))
                      tss)
            (lambda (lhs rhs)
              (time-less-p
               (org-time-string-to-time (org-element-property :raw-value lhs))
               (org-time-string-to-time (org-element-property :raw-value rhs))))))))

(cl-defun org-glance-datetime-capture (&rest _args)
  (setq-local -org-glance-datetime:local-timestamps
              (org-glance-datetime-active-repeated-timestamps)))

(cl-defun org-glance-datetime-restore (&rest _args)
  (let ((standard-output 'ignore)
        (tss* (org-glance-datetime-active-repeated-timestamps)))
    (cl-loop
       for ts in (cdr -org-glance-datetime:local-timestamps)
       for ts* in (cdr tss*)
       do (save-excursion
            (goto-char (org-element-property :begin ts*))
            (delete-region (org-element-property :begin ts*)
                           (org-element-property :end ts*))
            (insert (org-element-property :raw-value ts))))))

(provide 'org-glance-datetime-mode)
