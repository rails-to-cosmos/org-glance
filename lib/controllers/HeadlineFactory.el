;; -*- lexical-binding:t -*-

(require 'org-glance-module)

(org-glance:require lib.models.Scope
                    lib.models.Headline
                    lib.utils.helpers)

(cl-iter-defun org-glance-headline-factory:extract-from-buffer (&optional (buffer (current-buffer)))
  "Extract headlines from buffer BUFFER."
  (save-window-excursion
    (switch-to-buffer buffer)
    (cl-loop
       for element in (org-element-map (org-element-parse-buffer 'headline) 'headline 'identity)
       do
         (goto-char (org-element-property :begin element))
         (cl-loop for headline in (org-glance-headline:create-headlines-from-element-at-point)
            do (iter-yield headline)))))

(cl-iter-defun org-glance-headline-factory:extract-from-file (file)
  "Extract headlines from file FILE."
  (save-window-excursion
    (find-file file)
    (iter-yield-from (org-glance-headline-factory:extract-from-buffer))))

(cl-iter-defun org-glance-headline-factory:extract-from-scope (scope)
  (cl-loop for file in (org-glance-scope scope)
     for headline in (org-glance-headline-factory:extract-from-file file)
     do (iter-yield headline)))

;; (save-excursion
;;   (iter-do (headline (org-glance-headline-factory:extract-from-file "/tmp/hello.org"))
;;     (pp headline)))

;; (save-excursion
;;   (iter-do (headline (org-glance-headline-factory:extract-from-scope "/tmp"))
;;     (pp headline)))

(org-glance:provide)
