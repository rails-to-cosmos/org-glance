(require 'highlight)
(require 'org-glance-module)

(defvar org-glance-overview-mode-map (make-sparse-keymap)
  "Manipulate `org-mode' entries in `org-glance-overview-mode'.")

;; display global agenda: too global to use in overview mode
(define-key org-glance-overview-mode-map (kbd "A") 'org-glance-overview:agenda*)

;;; heavy methods applied to all headlines from current view's scope
;;; convention is to bind such methods to UPPERCASE KEYS

;; rebuild view and reread all files from view's scope
(define-key org-glance-overview-mode-map (kbd "G") 'org-glance-overview:pull*)

;;; medium methods applied for all first-level headlines in current file

;; lightweight methods applied for current headline
(define-key org-glance-overview-mode-map (kbd ";") 'org-glance-overview:comment)
(define-key org-glance-overview-mode-map (kbd "RET") 'org-glance-overview:visit)
(define-key org-glance-overview-mode-map (kbd "a") 'org-glance-overview:agenda)
(define-key org-glance-overview-mode-map (kbd "d") 'org-glance-overview:doctor)
(define-key org-glance-overview-mode-map (kbd "n") 'next-line)
(define-key org-glance-overview-mode-map (kbd "o") 'org-open-at-point)
(define-key org-glance-overview-mode-map (kbd "p") 'previous-line)
(define-key org-glance-overview-mode-map (kbd "q") 'bury-buffer)
(define-key org-glance-overview-mode-map (kbd "r") 'org-glance-overview:refer)
(define-key org-glance-overview-mode-map (kbd "v") 'org-glance-overview:visit)
(define-key org-glance-overview-mode-map (kbd "z") 'org-glance-overview:vizualize)

(define-key org-glance-overview-mode-map (kbd "C-c C-p") 'org-glance-edit-mode:start)

(define-minor-mode org-glance-overview-mode
    "A minor read-only mode to use in .org_summary files."
  nil nil org-glance-overview-mode-map
  (read-only-mode 'toggle))

(defvar org-glance-edit-mode-map (make-sparse-keymap)
  "Edit entries in `org-glance-edit-mode'.")

(define-key org-glance-edit-mode-map (kbd "C-c C-c") 'org-glance-edit-mode:apply)

(define-minor-mode org-glance-edit-mode
    "A minor mode to edit and sync .org_summary files."
  nil nil org-glance-edit-mode-map)

(cl-defun org-glance-edit-mode:start ()
  (interactive)
  (org-glance-edit-mode +1)
  (org-glance-overview-mode -1)
  (message "Edit mode is now enabled."))

(cl-defun org-glance-edit-mode:apply ()
  (interactive)
  (org-glance-edit-mode -1)
  (org-glance-overview-mode +1)
  (message "All changes have been applied."))

(cl-defun org-glance-overview:agenda ()
  (interactive)
  (let ((org-agenda-files (list (buffer-file-name))))
    (org-agenda-list)
    (org-agenda-day-view)))

(cl-defun org-glance-overview:agenda* ()
  (interactive)
  (let ((org-agenda-files (mapcar #'org-glance-view:summary-location (org-glance-view:ids))))
    (org-agenda-list)
    (org-agenda-month-view)))

(cl-defun org-glance-overview:visit ()
  (interactive)
  (if (org-before-first-heading-p)
      (message "not implemented yet")
    (->> (org-glance-headline:at-point)
      org-glance-headline:id
      org-glance-metastore:get-headline
      org-glance-headline:visit)))

(cl-defun org-glance-overview:doctor ()
  (interactive)
  (if (org-before-first-heading-p)
      (let* ((view-id (intern (org-get-category))))
        (org-glance-view:doctor view-id))
    (message "not implemented yet")))

(cl-defmacro org-glance-overview:for-all (view-id if-form &rest else-forms)
  (declare (indent 2) (debug t))
  `(if (org-before-first-heading-p)
       (let ((,view-id (intern (org-get-category))))
         ,if-form)
     ,@else-forms))

(cl-defun org-glance-overview:pull* ()
  (interactive)
  (let ((view-id (intern (org-get-category))))
    (when (y-or-n-p (org-glance:format "Update view ${view-id}?"))
      (kill-buffer)
      (org-glance-view:summary view-id)
      (message (org-glance:format "View ${view-id} is now up to date")))))

(cl-defun org-glance-overview:pull ()
  (interactive)
  (org-glance-overview:for-all view-id
      nil
    (let* ((inhibit-read-only t)
           (initial-point (point))
           (current-headline (org-glance-headline:at-point))
           (current-headline-id (org-glance-headline:id current-headline))
           (current-headline-title (org-glance-headline:format current-headline))
           (current-headline-indent (org-glance-headline:level current-headline))
           (current-headline-contents (org-glance-headline:contents current-headline))
           (original-headline (org-glance-metastore:get-headline current-headline-id))
           (original-headline-contents (org-glance-headline:contents original-headline)))
      (cond ((null original-headline-contents) (when (y-or-n-p "Original heading not found. Remove it?")
                                                 (kill-region (org-entry-beginning-position) (org-entry-end-position))))
            ((string= current-headline-contents original-headline-contents) (message (org-glance:format "Headline \"${current-headline-title}\" is up to date")))
            (t (save-excursion
                 (save-restriction
                   (org-glance-headline:get-or-search-backward)
                   (org-narrow-to-subtree)
                   (delete-region (point-min) (point-max))
                   (insert original-headline-contents)
                   (goto-char (point-min))
                   (cl-loop for i from 1 to (1- current-headline-indent)
                      do (org-demote-subtree))
                   (org-content)))
               (goto-char initial-point)
               (save-buffer))))))

(cl-defun org-glance-overview:comment ()
  (interactive)
  (save-window-excursion
    (->> (org-glance-headline:at-point)
      (org-glance-headline:id)
      (org-glance-metastore:get-headline)
      (org-glance-headline:visit))
    (org-toggle-comment)
    (save-buffer))
  (org-glance-overview:pull))

(cl-defun org-glance-overview:edit-mode ()
  (interactive)

  (org-glance-overview:for-all view-id
      nil
    (let* ((headline (org-glance-headline:at-point))
           (beg (org-element-property :begin (org-glance-headline:at-point)))
           (end (org-element-property :end (org-glance-headline:at-point))))
      (hlt-unhighlight-region beg end)
      ;; (hlt-highlight-region beg end 'expal-block-hover-face)
      (remove-text-properties beg end '(read-only t)))))

(cl-defun org-glance-overview:original-headline ()
  (org-glance-headline:narrow
      (->> (org-glance-headline:at-point)
        org-glance-headline:id
        org-glance-metastore:get-headline)
    (org-glance-headline:at-point)))

(cl-defun org-glance-overview:refer ()
  (interactive)
  (let ((source (org-glance-overview:original-headline))
        (target (org-glance-metastore:choose-headline)))
    (org-glance:add-relation source "Related to" target)
    (org-glance:add-relation target "Referred from" source)
    (org-glance-overview:pull)))

(cl-defun org-glance-overview:vizualize ()
  (interactive)
  (org-glance-overview:for-all view-id
      (error "not implemented yet") ;; org-glance-headline:scan-file
    (let ((relations (org-glance-headline:relations*)))
      (with-temp-file "relations.js"
        (insert "var relations = ["
                (s-join "," (cl-loop
                               for rel in relations
                               for name = (car rel)
                               for relations = (s-join "," (mapcar (-rpartial #'s-wrap "\"") (cdr rel)))
                               collect (org-glance:format "{\"name\":\"${name}\",\"relations\":[${relations}]}")))
                "];")))))

(org-glance-module-provide)
