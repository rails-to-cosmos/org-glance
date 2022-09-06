;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'cl-macs)
(require 'thunk)
(require 'highlight)

(require 'org-glance-headline)
(require 'org-glance-mew)
(require 'org-glance-store)

(defvar org-glance-material-mode-map (make-sparse-keymap)
  "Extend `org-mode' map with synchronization abilities.")

(define-minor-mode org-glance-material-mode
    "A minor mode to be activated only in materialized view
editor."
  nil nil org-glance-material-mode-map
  (cond (org-glance-material-mode
         (org-glance-mew:mark)
         (org-glance-mew:fetch)
         (add-hook 'post-command-hook #'org-glance-material-mode:debug nil t)
         (add-hook 'after-change-functions #'org-glance-material-mode:update nil t)
         (add-hook 'before-save-hook #'org-glance-mew:commit nil t))
        (t
         (remove-hook 'before-save-hook #'org-glance-mew:commit t)
         (remove-hook 'after-change-functions #'org-glance-material-mode:update t)
         (remove-hook 'post-command-hook #'org-glance-material-mode:debug t))))

(cl-defun org-glance-material-mode:update (change-beg change-end pre-change-length)
  "Actualize marker overlay."
  (interactive)
  (org-glance-message "** Change Event")
  (org-glance-message "   Consistent: %s" (org-glance-mew:consistent-p))
  (org-glance-message "   Change params: \"%s\"" (list change-beg change-end pre-change-length))
  (org-glance-message "   Change region: \"%s\"" (buffer-substring-no-properties change-beg change-end))
  (condition-case nil
      (org-glance-message "   Delete region: \"%s\"" (buffer-substring-no-properties change-beg (+ change-beg pre-change-length)))
    (error (org-glance-message "   Delete region doesn't exist anymore")))

  (org-glance-message "   Consistent: %s" (org-glance-mew:consistent-p))
  (let ((marker-pos-before (cl-copy-seq (org-glance-> (org-glance-mew:current) :marker-positions)))
        marker-pos-after)
    (org-glance-message "   Marker positions BEFORE: %s" marker-pos-before)

    (thunk-let* ((mew (org-glance-mew:current))
                 (midx (org-glance-mew:marker-at-point mew change-beg))
                 (diff (- (- change-end change-beg) pre-change-length)))
      (org-glance-mew:set-marker-changed mew midx t)
      (org-glance-mew:shift-markers mew midx diff))

    (setq marker-pos-after (cl-copy-seq (org-glance-> (org-glance-mew:current) :marker-positions)))
    (org-glance-message "   Marker positions AFTER: %s" marker-pos-after)
    (org-glance-message "   Marker positions DIFF: %s" (cl-loop for i from 0 below (length marker-pos-before)
                                               collect (- (aref marker-pos-after i) (aref marker-pos-before i))))
    (org-glance-message "   Consistent: %s" (org-glance-mew:consistent-p))))

(cl-defun org-glance-material-mode:debug (&rest _)
  (let ((mew (org-glance-mew:current))
        (midx (org-glance-mew:marker-at-point)))
    (hlt-unhighlight-region)

    (org-glance-debug
     (point)
     midx
     (format "Changed: %s" (org-glance-mew:marker-changed-p mew midx))
     (format "Corrupted: %s" (org-glance-mew:marker-corrupted-p mew midx))
     (format "Committed: %s" (org-glance-mew:marker-committed-p mew midx))
     (org-glance-mew:get-marker-position mew midx)
     (org-glance-mew:get-marker-position mew (1+ midx))
     (org-glance-> mew :marker-positions))

    (when (> midx -1)
      (hlt-highlight-region (org-glance-mew:get-marker-position mew midx)
                            (org-glance-mew:get-marker-position mew (1+ midx))
                            'region))
    ))

(provide 'org-glance-material-mode)
