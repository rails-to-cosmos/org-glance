(require 'org-glance-module)

;; (plist-get '(:a 1 :b 2) :a)

(cl-defun org-glance-headline:choose ()
  (let ((headlines
         (cl-loop for vid in (org-glance-view:ids)
            append (cl-loop for hl in (org-glance-view:headlines vid)
                      collect (cons  ;; duplication of format*
                               (format "[%s] %s" vid (org-glance-headline:format hl))
                               hl)))))
    (alist-get (org-completing-read "Headline: " headlines) headlines nil nil #'string=)))

(cl-defun org-glance-headline:title (hl)
  (org-element-property :raw-value hl))

(cl-defun org-glance:add-relation (&optional (hl (org-glance-headline:choose)))
  (interactive)
  (unless (looking-at "^\\W*$")
    (end-of-line)
    (newline))

  (insert (format "- Relates to [[elisp:(org-glance-headline:visit \"%s\")][%s]]"
                  (org-glance-headline:id hl)
                  (org-glance-headline:title hl)))

  ;; relates to / depends on / etc?
  ;; maybe implement directed graph in a future

  ;; (when (and (string= major-mode "org-mode")
  ;;            (eq this-command 'org-self-insert-command)
  ;;            (looking-back "^\\W*Glance\\." 6))
  ;;   (condition-case nil

  ;;       (quit nil)))
  )

(org-glance-module-provide)
