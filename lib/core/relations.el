(require 'org-glance-module)

(defun org-glance:add-relation ()
  (interactive)
  (let* ((view-id (org-glance-view:completing-read))
         (view (org-glance-view:get-view-by-id view-id))
         (headlines (org-glance-view:headlines view))
         ;; TODO extract headline id
         (headline (org-glance-scope--prompt-headlines "Headline: " headlines)))
    ;; (beginning-of-line)
    ;; (kill-line)
    (insert (format "- [[Relates]] to =%s=: [[%s]]" view-id headline)))

  ;; (when (and (string= major-mode "org-mode")
  ;;            (eq this-command 'org-self-insert-command)
  ;;            (looking-back "^\\W*Glance\\." 6))
  ;;   (condition-case nil

  ;;       (quit nil)))
  )

(org-glance-module-provide)
