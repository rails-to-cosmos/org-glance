(defvar org-glance-scratch-mode-map (make-sparse-keymap)
  "Manipulate `org-mode' entries in `org-glance-scratch-mode'.")

(cl-defun org-glance-scratch-location ()
  (f-join org-glance-directory "glance.org"))

(define-minor-mode org-glance-scratch-mode
    "A minor read-only mode to use in glance scratch files."
  nil nil org-glance-overview-mode-map
  (read-only-mode 'toggle))

(cl-defun org-glance-scratch:create ()
  (interactive)
  (with-temp-file (org-glance-scratch-location)
    (let ((inhibit-read-only t)
          (calendar (org-glance-overview:calendar-widget)))
      (insert calendar))))
