;; -*- lexical-binding:t -*-

(require 'org-glance-module)

(cl-iter-defun org-glance-headline-factory.parse-buffer ()
  "Extract headlines from current buffer."
  (cl-loop
     for element in (org-element-map (org-element-parse-buffer 'headline) 'headline #'identity)
     do
       (goto-char (org-element-property :begin element))
       (cl-loop for headline in (org-glance-headline:create-headlines-from-element-at-point)
          do (iter-yield headline))))

(cl-iter-defun org-glance-headline-factory:extract-from-scope (scope)
  (cl-loop for file in (org-glance-scope scope)
     for headline in (org-glance-headline-factory:extract-from-file file)
     do (iter-yield headline)))

(org-glance:provide)
