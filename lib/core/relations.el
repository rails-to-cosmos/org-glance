(require 'org-glance-module)

(defun org-glance:add-relation ()
  (interactive)
  (let* ((headlines
          (cl-loop for view being the hash-values of org-glance-views
             append (org-glance-view:headlines/formatted view)))
         ;; TODO extract headline id
         (headline (org-completing-read "Headline: " (sort headlines #'s-less?))))
    (unless (looking-at "^\\W*$")
      (end-of-line)
      (newline))
    (insert (format "- Relates to [[%s]]" headline)))

  ;; relates to / depends on / etc?
  ;; maybe implement directed graph in a future

  ;; (when (and (string= major-mode "org-mode")
  ;;            (eq this-command 'org-self-insert-command)
  ;;            (looking-back "^\\W*Glance\\." 6))
  ;;   (condition-case nil

  ;;       (quit nil)))
  )

(org-glance-module-provide)
