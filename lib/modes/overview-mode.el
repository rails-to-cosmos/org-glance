(require 'highlight)
(require 'org-attach)
(require 'org-glance-module)

(defconst org-glance-overview:header "#    -*- mode: org; mode: org-glance-overview -*-

#+CATEGORY: ${category}
#+STARTUP: overview
#+LATEST_CHANGE: ?

")

(defvar org-glance-overview-mode-map (make-sparse-keymap)
  "Manipulate `org-mode' entries in `org-glance-overview-mode'.")

;; display global agenda: too global to use in overview mode
(define-key org-glance-overview-mode-map (kbd "A") 'org-glance-overview:agenda*)

;;; heavy methods applied to all headlines from current view's scope
;;; convention is to bind such methods to UPPERCASE KEYS

;; rebuild view and reread all files from view's scope
(define-key org-glance-overview-mode-map (kbd "G") 'org-glance-overview:pull!)

;;; medium methods applied for all first-level headlines in current file

;; lightweight methods applied for current headline
(define-key org-glance-overview-mode-map (kbd ";") 'org-glance-overview:comment)
(define-key org-glance-overview-mode-map (kbd "<") 'beginning-of-buffer)
(define-key org-glance-overview-mode-map (kbd ">") 'end-of-buffer)
(define-key org-glance-overview-mode-map (kbd "@") 'org-glance-overview:refer)
(define-key org-glance-overview-mode-map (kbd "RET") 'org-glance-overview:visit-headline)
(define-key org-glance-overview-mode-map (kbd "a") 'org-glance-overview:agenda)
(define-key org-glance-overview-mode-map (kbd "d") 'org-glance-overview:doctor)
(define-key org-glance-overview-mode-map (kbd "f") #'org-attach-reveal-in-emacs)
(define-key org-glance-overview-mode-map (kbd "g") #'(lambda ()
                                                       (interactive)
                                                       (org-glance-overview:for-all
                                                           (org-glance-overview:pull*)
                                                         (org-glance-overview:pull))))
(define-key org-glance-overview-mode-map (kbd "n") 'org-glance-headline:search-forward)
(define-key org-glance-overview-mode-map (kbd "o") 'org-open-at-point)
(define-key org-glance-overview-mode-map (kbd "p") 'org-glance-headline:search-backward)
(define-key org-glance-overview-mode-map (kbd "q") 'bury-buffer)
(define-key org-glance-overview-mode-map (kbd "v") 'org-glance-overview:visit-headline)
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

(cl-defun org-glance-overview:location (&optional (vid (org-glance-view:completing-read)))
  "Path to file where VIEW-ID exported headlines are stored."
  (let ((view-name (s-downcase (format "%s" vid))))
    (f-join org-glance-view-location
            view-name
            (format "%s.org_summary" view-name))))

(cl-defun org-glance-overview:sort-buffer (&optional (start (point-min)))
  "Sort headlines by todo state, then sort each group by time.

TODO: implement unit tests."
  (org-save-outline-visibility nil
    (save-excursion
      (goto-char start)
      (unless (org-glance-headline-p) (org-glance-headline:search-forward))
      (let* ((state (org-glance-headline:state))
             (beginning-of-region (point))
             (end-of-region (cl-loop
                               initially (org-glance-headline:search-forward)
                               while (< (point) (point-max))
                               while (string= (org-glance-headline:state) state)
                               do (org-glance-headline:search-forward)
                               finally (return (point)))))
        (set-mark beginning-of-region)
        (goto-char end-of-region)
        (org-sort-entries nil ?p)
        (when (< end-of-region (point-max))
          (org-glance-overview:sort-buffer end-of-region))))))

(cl-defun org-glance-overview:create (&optional (view-id (org-glance-view:completing-read)))
  (interactive)
  (let* ((filename (org-glance-overview:location view-id))
         (category view-id)
         (header (org-glance:format org-glance-overview:header))
         (headlines (->> view-id org-glance-view:update org-glance-view:headlines))
         (inhibit-read-only t))
    (--org-glance:make-file-directory filename)
    (with-temp-file filename
      (insert header)
      (insert (s-join "\n" (mapcar #'org-glance-headline:contents headlines)))
      (org-mode)
      (goto-char (point-min))
      (set-mark (point-max))
      (org-sort-entries nil ?o)
      (org-glance-overview:sort-buffer)
      (org-align-tags t))
    (find-file filename)))

(cl-defun org-glance-overview:visit (&optional (view-id (org-glance-view:completing-read)))
  (interactive)
  (let ((location (org-glance-overview:location view-id)))
    (if (file-exists-p location)
        (find-file location)
      (org-glance-overview:create view-id))))

(cl-defun org-glance-overview:agenda ()
  (interactive)
  (let ((org-agenda-files (list (buffer-file-name))))
    (org-agenda-list)
    (org-agenda-day-view)))

(cl-defun org-glance-overview:agenda* ()
  (interactive)
  (let ((org-agenda-files (mapcar #'org-glance-overview:location (org-glance-view:ids))))
    (org-agenda-list)
    (org-agenda-month-view)))

(cl-defun org-glance-overview:visit-headline ()
  (interactive)
  (org-glance-overview:for-all
      (error "not implemented")
    (let ((offset (- (point) (save-excursion
                               (org-glance-headline:goto-beginning-of-current-headline)
                               (point)))))
      (-some->> (org-glance-headline:at-point)
        org-glance-headline:id
        org-glance-metastore:get-headline
        org-glance-headline:visit)
      (forward-char offset))))

(cl-defun org-glance-overview:doctor ()
  (interactive)
  (if (org-before-first-heading-p)
      (let* ((view-id (intern (org-get-category))))
        (org-glance-view:doctor view-id))
    (message "not implemented yet")))

(cl-defmacro org-glance-overview:for-all (then &rest else)
  (declare (indent 1) (debug t))
  `(if (org-before-first-heading-p)
       ,then
     ,@else))

(cl-defun org-glance-overview:pull! ()
  "Completely rebuild current overview file."
  (interactive)
  (let ((view-id (intern (org-get-category))))
    (when (y-or-n-p (org-glance:format "Update view ${view-id}?"))
      (kill-buffer)
      (org-glance-overview:create view-id)
      (message (org-glance:format "View ${view-id} is now up to date")))))

(cl-defun org-glance-overview:pull* ()
  "Apply `org-glance-overview:pull' to each headline in current overview file."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (org-glance-headline:search-forward)
      (org-glance-overview:pull))))

(cl-defun org-glance-overview:pull ()
  "Pull any modifications from original headline to it's overview clone at point."
  (interactive)
  (let* ((inhibit-read-only t)
         (initial-point (point))
         (current-headline (org-glance-headline:at-point))
         (current-headline-id (org-glance-headline:id current-headline))
         (current-headline-title (org-glance-headline:format current-headline))
         (current-headline-indent (org-glance-headline:level current-headline))
         (current-headline-contents (org-glance-headline:contents current-headline))
         (original-headline (org-glance-metastore:get-headline current-headline-id))
         (original-headline-contents (org-glance-headline:contents original-headline)))
    (cond ((null original-headline-contents)
           (when (y-or-n-p (org-glance:format "Original headline for \"${current-headline-title}\" not found. Remove it?"))
             (kill-region (org-entry-beginning-position) (org-entry-end-position))))
          ((string= current-headline-contents original-headline-contents)
           (condition-case nil
               (message (org-glance:format "Headline \"${current-headline-title}\" is up to date"))
             (error (message "Headline is up to date"))))
          (t (save-excursion
               (save-restriction
                 (condition-case nil
                     (message (org-glance:format "Headline \"${current-headline-title}\" has been changed"))
                   (error (message "Original headline has been changed")))
                 (org-glance-headline:goto-beginning-of-current-headline)
                 (org-narrow-to-subtree)
                 (delete-region (point-min) (point-max))
                 (insert original-headline-contents)
                 (goto-char (point-min))
                 (cl-loop
                    for i from 1 to (1- current-headline-indent)
                    do (org-demote-subtree))
                 (org-content)))
             (goto-char initial-point)
             (save-buffer)))))

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
  (org-glance-overview:for-all
      (error "not implemented yet")
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
    (org-glance:add-relation source org-glance-relation:forward target)
    (org-glance:add-relation target org-glance-relation:backward source)
    (org-glance-overview:pull)))

(cl-defun org-glance-overview:vizualize ()
  (interactive)
  (org-glance-overview:for-all
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
