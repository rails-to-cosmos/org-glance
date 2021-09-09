(require 'highlight)
(require 'org-attach)
(require 'org-glance-module)

(org-glance:require
  highlight
  org-attach)

(defconst org-glance-overview:header "#    -*- mode: org; mode: org-glance-overview -*-

#+CATEGORY: ${category}
#+STARTUP: overview

")

(defvar org-glance-overview-mode-map (make-sparse-keymap)
  "Manipulate `org-mode' entries in `org-glance-overview-mode'.")

;;; heavy methods applied to all headlines from current view's scope
;;; convention is to bind such methods to UPPERCASE KEYS

;; rebuild view and reread all files from view's scope
(define-key org-glance-overview-mode-map (kbd "G") 'org-glance-overview:pull!)

;;; medium methods applied for all first-level headlines in current file

(defmacro org-glance-overview:context-aware-lambda (&rest forms)
  "Eval FORMS on headline at point.
If point is before first heading, eval forms on each headline."
  (declare (indent 0) (debug t))
  `(lambda ()
     (interactive)
     (if (org-before-first-heading-p)
         (progn
           (goto-char (point-min))
           (while (and (org-glance-headline:search-forward) (sit-for 0.01))
             (when (= (org-glance-headline:level) 1)
               ,@forms)))
       ,@forms)))

;; lightweight methods applied for current headline
(define-key org-glance-overview-mode-map (kbd ";") #'org-glance-overview:archive)
(define-key org-glance-overview-mode-map (kbd "<") #'beginning-of-buffer)
(define-key org-glance-overview-mode-map (kbd ">") #'end-of-buffer)
(define-key org-glance-overview-mode-map (kbd "^") #'(lambda ()
                                                       (interactive)
                                                       (save-excursion
                                                         (let ((inhibit-read-only t))
                                                           (goto-char (point-min))
                                                           (org-glance-overview:sort)
                                                           (org-overview)
                                                           (save-buffer)))))
(define-key org-glance-overview-mode-map (kbd "@") #'org-glance-overview:add-relation)
(define-key org-glance-overview-mode-map (kbd "RET") #'org-glance-overview:materialize-headline)
(define-key org-glance-overview-mode-map (kbd "a") #'org-glance-overview:agenda)
(define-key org-glance-overview-mode-map (kbd "f") #'org-attach-reveal-in-emacs)
(define-key org-glance-overview-mode-map (kbd "g")
  (org-glance-overview:context-aware-lambda
    (org-glance-overview:doctor)))
(define-key org-glance-overview-mode-map (kbd "n") #'org-glance-headline:search-forward)
(define-key org-glance-overview-mode-map (kbd "o") #'org-open-at-point)
(define-key org-glance-overview-mode-map (kbd "p") #'org-glance-headline:search-backward)
(define-key org-glance-overview-mode-map (kbd "q") #'bury-buffer)
(define-key org-glance-overview-mode-map (kbd "v") #'org-glance-overview:visit-headline)
(define-key org-glance-overview-mode-map (kbd "k") #'org-glance-overview:kill-headline)
(define-key org-glance-overview-mode-map (kbd "z") #'org-glance-overview:vizualize)

(define-key org-glance-overview-mode-map (kbd "C-c C-p") #'org-glance-edit-mode:start)

(define-key org-glance-overview-mode-map (kbd "+") #'(lambda () (interactive) (org-glance-overview:capture (org-glance-overview:category))) )
(define-key org-glance-overview-mode-map (kbd "*") #'org-glance-overview:import-headlines)
(define-key org-glance-overview-mode-map (kbd "/") #'org-glance-overview:select-headline)

(cl-defun org-glance-overview:register-headline-in-metastore (headline view-id)
  (let* ((metastore-location (-some->> view-id
                               org-glance-view:get-view-by-id
                               org-glance-view:metastore-location))
         (metastore (org-glance-metastore:read metastore-location)))
    (org-glance-metastore:add-headline headline metastore)
    (org-glance-metastore:write metastore-location metastore)))

(cl-defun org-glance-overview:register-headline-in-overview (headline view-id)
  "Add HEADLINE clone in overview VIEW-ID file."
  (save-window-excursion
    (org-glance-overview view-id)
    (condition-case nil
        (progn
          (org-glance-headline:search-buffer-by-id (org-glance-headline:id headline))
          (org-glance-overview:pull))
      (error (let ((inhibit-read-only t)
                   (contents (org-glance-headline:contents headline)))
               (unless (string-empty-p contents)
                 (beginning-of-buffer)
                 (org-glance-headline:search-forward)
                 (insert contents "\n")
                 (save-buffer))))))


  headline)

(cl-defun org-glance:capture-headline-at-point
    (&optional (view-id (org-completing-read "Capture headline for view: " (org-glance-view:ids)))
     &key (remove-original t))
  (save-window-excursion
    (save-excursion
      (org-glance:ensure-at-heading)
      (let* ((view-id (cond ((symbolp view-id) (symbol-name view-id))
                            ((stringp view-id) view-id)))
             (id (org-glance-view:generate-id-for-subtree-at-point view-id))
             (dir (org-glance:generate-dir-for-subtree-at-point view-id))
             (output-file (f-join dir (org-glance:format "${view-id}.org"))))
        (mkdir dir 'parents)

        (save-restriction
          (org-narrow-to-subtree)
          (let* ((contents (buffer-substring-no-properties (point-min) (point-max)))
                 (result (save-window-excursion
                           (find-file output-file)
                           (save-restriction
                             (widen)
                             (end-of-buffer)
                             (save-excursion
                               (insert contents))
                             (org-glance-headline:promote-to-the-first-level)
                             (org-set-property "ORG_GLANCE_ID" id)
                             (org-set-property "DIR" dir)
                             (org-set-property "CATEGORY" view-id)
                             (org-set-property "ORG_GLANCE_CREATION_TIME" (with-temp-buffer
                                                                            (let ((current-prefix-arg '(16)))
                                                                              (call-interactively #'org-time-stamp-inactive)
                                                                              (buffer-substring-no-properties (point-min) (point-max)))))
                             (unless (member (downcase view-id) (-org-glance:collect-tags))
                               (org-toggle-tag view-id))
                             (save-buffer)
                             (org-glance-headline:at-point)))))
            (when remove-original
              (delete-region (point-min) (point-max)))
            result))))))

(cl-defun org-glance-overview:capture
    (&optional
       (view-id (org-glance-view:choose))
       (title (read-string (format "New %s: " view-id))))
  (interactive)
  (org-glance-overview view-id)
  (let ((captured-headline (with-temp-buffer
                             (insert "* " title)
                             (org-glance:capture-headline-at-point view-id))))
    (org-glance-overview:register-headline-in-metastore captured-headline view-id)
    (org-glance-overview:register-headline-in-overview captured-headline view-id)
    (org-overview)
    (org-glance-headline:search-buffer-by-id (org-glance-headline:id captured-headline))
    (org-glance-overview:materialize-headline)
    captured-headline))

(cl-defun org-glance-overview:import-headlines
    (path
     &optional
       (view-id (org-glance-overview:category)))
  (interactive "fChoose file or directory to import: ")
  (when (y-or-n-p (org-glance:format "Import headlines of class ${view-id} from ${path}?"))
    (cl-loop
       for original-headline in (cl-loop
                                   for file in (org-glance-scope path)
                                   append (-non-nil (mapcar (org-glance-view-filter (org-glance-view:get-view-by-id view-id))
                                                            (progn
                                                              (message "Scan file %s" file)
                                                              (redisplay)
                                                              (with-temp-buffer
                                                                (org-mode)
                                                                (insert-file-contents file)
                                                                (let ((filename (abbreviate-file-name file)))
                                                                  (org-element-map (org-element-parse-buffer 'headline) 'headline
                                                                    (lambda (el)
                                                                      (org-glance-headline:enrich el :file filename)))))))))
       do (let ((pos (org-glance-headline:begin original-headline))
                (file (org-glance-headline:file original-headline)))
            (find-file file)
            (goto-char pos)
            (let ((captured-headline (org-glance:capture-headline-at-point view-id :remove-original nil)))
              (org-glance-overview:register-headline-in-metastore captured-headline view-id)
              (org-glance-overview:register-headline-in-overview captured-headline view-id))))))

(cl-defun org-glance-overview:select-headline
    (&optional (view-id (org-glance-overview:category)))
  (interactive)
  (let ((headlines (org-glance-view:headlines view-id)))
    (org-glance-headline:search-buffer-by-id
     (org-glance-headline:id (org-glance-scope--choose-headline
                              (org-glance-scope--prompt-headlines "Choose: " headlines)
                              headlines)))))

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

(cl-defun org-glance-overview:directory (&optional (view-id (org-glance-view:completing-read)))
  "Path to file where VIEW-ID headlines are stored."
  (let ((view-name (s-downcase (format "%s" view-id))))
    (abbreviate-file-name
     (f-join org-glance-directory view-name))))

(cl-defun org-glance-overview:location (&optional
                                          (view-id (org-glance-view:completing-read))
                                          (extension "org_summary"))
  "Path to file where VIEW-ID headlines are stored."
  (let ((view-name (s-downcase (format "%s" view-id))))
    (abbreviate-file-name
     (f-join org-glance-directory
             view-name
             (org-glance:format "${view-name}.${extension}")))))

(cl-defun org-glance-overview:sorting-by-type (sorting-type)
  "Determine how to group entries by `org-sort-entries' SORTING-TYPE."
  (case (if (listp sorting-type) (car sorting-type) sorting-type)
    (?a #'org-glance-headline:title)
    (?p #'org-glance-headline:priority)
    (?c #'org-glance-headline:creation-time)
    (?o #'org-glance-headline:state)
    (?t #'org-glance-headline:creation-time)
    (?f (cadr sorting-type))
    (t nil)))

(cl-defun org-glance-overview:comparator-by-type (sorting-type)
  "Determine how to compare entries by `org-sort-entries' SORTING-TYPE."
  (case (if (listp sorting-type) (car sorting-type) sorting-type)
    (?a #'string=)
    (?p #'eql)
    (?c #'string=)
    (?o #'string=)
    (?t #'string=)
    (?f #'eql)
    (t nil)))

(cl-defun org-glance-overview:sort (&optional
                                      (order '(
                                               (?f ;; move commented headings down
                                                (lambda () (if (org-in-commented-heading-p t) 1 -1))
                                                <)
                                               (?f ;; move archived headings down
                                                (lambda () (if (org-in-archived-heading-p) 1 -1))
                                                <)
                                               ?o
                                               ?p))
                                      group)
  ;; a   Alphabetically, ignoring the TODO keyword and the priority, if any.
  ;; c   By creation time, which is assumed to be the first inactive time stamp
  ;;     at the beginning of a line.
  ;; d   By deadline date/time.
  ;; k   By clocking time.
  ;; n   Numerically, by converting the beginning of the entry/item to a number.
  ;; o   By order of TODO keywords.
  ;; p   By priority according to the cookie.
  ;; r   By the value of a property.
  ;; s   By scheduled date/time.
  ;; t   By date/time, either the first active time stamp in the entry, or, if
  ;;     none exist, by the first inactive one.

  (cond ((null order) nil)
        ((null group) (progn
                        (apply #'org-sort-entries
                               (append '(nil)
                                       (if (listp (car order)) (car order) (list (car order)))))
                        (org-glance-overview:sort (cdr order) (car order))))
        (t (let ((grouper (org-glance-overview:sorting-by-type group))
                 (comparator (org-glance-overview:comparator-by-type group)))
             (beginning-of-buffer)
             (org-glance-headline:search-forward)
             (while (< (point) (point-max))
               (let* ((group-state (funcall grouper))
                      (beginning-of-group (point))
                      (end-of-group (cl-loop
                                       while (and (< (point) (point-max))
                                                  (funcall comparator group-state (funcall grouper)))
                                       do (org-glance-headline:search-forward)
                                       finally (return (point)))))
                 (set-mark beginning-of-group)
                 (goto-char end-of-group)
                 (apply #'org-sort-entries
                        (append '(nil)
                                (if (listp (car order)) (car order) (list (car order)))))
                 (goto-char end-of-group)))
             (org-glance-overview:sort (cdr order) (car order))))))

(cl-defun org-glance-overview:create (&optional (view-id (org-glance-view:completing-read)))
  (interactive)
  (let* ((inhibit-read-only t)
         (filename (org-glance-overview:location view-id))
         (category view-id)
         (header (org-glance:format org-glance-overview:header))
         (headlines (->> view-id org-glance-view:update org-glance-view:headlines))
         (contents (s-join "\n" (mapcar #'org-glance-headline:contents headlines))))
    (--org-glance:make-file-directory filename)
    (with-temp-file filename
      (org-mode)
      (insert header)
      (insert contents)
      (goto-char (point-min))
      (condition-case nil
          (org-glance-overview:sort)
        (user-error nil))
      (org-align-tags t))
    (find-file filename)))

(cl-defun org-glance-overview (&optional (view-id (org-glance-view:completing-read)))
  (interactive)
  (let ((location (org-glance-overview:location view-id)))
    (if (file-exists-p location)
        (find-file location)
      (org-glance-overview:create view-id))))

(cl-defun org-glance-overview:agenda ()
  (interactive)
  (let ((org-agenda-files (list (org-glance-overview:for-all
                                    (buffer-file-name)
                                  (org-glance-headline:file (org-glance-overview:original-headline))))))
    (org-agenda-list)
    (org-agenda-fortnight-view)))

(cl-defun org-glance-overview:agenda* ()
  (interactive)
  (let ((org-agenda-files (mapcar #'org-glance-overview:location (org-glance-view:ids))))
    (org-agenda-list)
    (org-agenda-fortnight-view)))

(cl-defun org-glance-overview:materialize-headline ()
  (interactive)
  (org-glance-overview:for-all
      nil
    (org-glance-action-call
     'materialize
     :on (org-glance-overview:original-headline)
     :for (org-glance-view-type (org-glance-view:get-view-by-id (org-glance-overview:category))))))

(cl-defun org-glance-overview:visit-headline ()
  (interactive)
  (org-glance-overview:for-all
      nil
    (let ((offset (- (point) (save-excursion
                               (org-glance-headline:goto-beginning-of-current-headline)
                               (point)))))
      (-some->> (org-glance-headline:at-point)
        org-glance-headline:id
        org-glance-metastore:get-headline
        org-glance-headline:visit)
      (forward-char offset))))

(cl-defun org-glance-overview:category ()
  (save-excursion
    (goto-char (point-min))
    (intern (org-get-category))))

(defmacro org-glance-doctor:fix-when (predicate prompt &rest forms)
  (declare (indent 2) (debug t))
  `(when (and ,predicate (or current-prefix-arg (y-or-n-p (org-glance:format ,prompt))))
     ,@forms))

(cl-defun org-glance-overview:doctor ()
  ;; - [ ] check if visited file is not headline archive file
  ;; - [ ] check for view data structure: no empty directories etc
  ;; - [x] check for view data structure: proper partitioning
  ;; - [ ] check for nested views and ask to flatten them
  ;; - [ ] check if original headline is stored in archive
  ;; - [ ] check for PROPERTIES drawer indentation

  (when (org-glance-overview:pull)
    (let* ((view-id (org-glance-overview:category))
           (original-headline (org-glance-overview:original-headline))
           (original-headline-location (org-glance-headline:file original-headline))
           (located-in-view-dir-p (cl-loop
                                     for view-id in (org-glance-headline:view-ids)
                                     for overview-file-name = (org-glance-overview:location view-id)
                                     for overview-location = (file-name-directory overview-file-name)
                                     for common-parent = (f-common-parent (list overview-location original-headline-location))
                                     when (string= (abbreviate-file-name common-parent)
                                                   (abbreviate-file-name overview-location))
                                     do (return t)))
           (title (org-glance-headline:title))
           (raw-value (org-glance-headline:raw-value original-headline))
           (now (format-time-string (org-time-stamp-format 'long 'inactive) (current-time))))

      (org-glance-doctor:fix-when (s-matches? org-link-any-re raw-value)
          "Headline \"${title}\" contains link in raw value. Move it to the body?"
        (org-glance-headline:rename original-headline (org-glance:clean-title raw-value))
        (org-glance-headline:narrow original-headline
          (org-end-of-meta-data t)
          (insert raw-value "\n")
          (save-buffer)))

      (org-glance-doctor:fix-when (not located-in-view-dir-p)
          "Headline \"${title}\" is located outside of ${view-id} directory: ${original-headline-location}. Capture it?"
        (let ((captured-headline (org-glance-headline:narrow original-headline
                                   (org-glance:capture-headline-at-point view-id))))
          (org-glance-overview:register-headline-in-metastore captured-headline view-id)
          (org-glance-overview:register-headline-in-overview captured-headline view-id))
        (org-glance-overview:register-headline-in-metastore (org-glance-overview:original-headline) view-id))

      (org-glance-overview:pull))))

(cl-defmacro org-glance-overview:for-all (then &rest else)
  (declare (indent 1) (debug t))
  `(if (org-before-first-heading-p)
       ,then
     ,@else))

(cl-defun org-glance-overview:pull! ()
  "Completely rebuild current overview file."
  (interactive)
  (let ((view-id (org-glance-overview:category)))
    (when (y-or-n-p (org-glance:format "Update view ${view-id}?"))
      (save-buffer)
      (kill-buffer)
      (org-glance-overview:create view-id)
      (message (org-glance:format "View ${view-id} is now up to date")))))

(cl-defun org-glance-overview:pull* ()
  "Apply `org-glance-overview:pull' to each headline in current overview file."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (and (org-glance-headline:search-forward))
      (org-glance-overview:pull))))

(cl-defun org-glance-overview:kill-headline (&optional force)
  "Remove `org-glance-headline' from overview, don't ask to confirm if FORCE is t."
  (interactive)
  (org-glance-headline:goto-beginning-of-current-headline)
  (let ((inhibit-read-only t)
        (view-id (org-glance-overview:category))
        (current-headline-title (org-glance-headline:title)))
    (when (and (org-glance-headline-p)
               (or force
                   (y-or-n-p (org-glance:format "Remove headline \"${current-headline-title}\" from ${view-id} overview?"))))
      (kill-region (org-entry-beginning-position) (org-entry-end-position)))))

(cl-defun org-glance-overview:pull ()
  "Pull any modifications from original headline to it's overview clone at point."
  (interactive)
  (let* ((inhibit-read-only t)
         (initial-point (point))
         (current-headline (org-glance-headline:at-point))
         (current-headline-title (org-glance-headline:title current-headline))
         (current-headline-indent (org-glance-headline:level current-headline))
         (current-headline-contents (org-glance-headline:contents current-headline))
         (original-headline (org-glance-overview:original-headline))
         (original-headline-contents (org-glance-headline:contents original-headline))
         )
    (cond
      ((null original-headline-contents)
       (if (y-or-n-p (org-glance:format "Original headline for \"${current-headline-title}\" not found. Remove it from overview?"))
           (org-glance-overview:kill-headline 'force)
         (org-glance-exception:headline-not-found "Original headline not found"))
       nil)
      ((string= current-headline-contents original-headline-contents)
       (message (org-glance:format "Headline \"${current-headline-title}\" is up to date"))
       t)
      (t (save-excursion
           (save-restriction
             (org-glance-headline:goto-beginning-of-current-headline)
             (org-narrow-to-subtree)
             (delete-region (point-min) (point-max))
             (insert original-headline-contents)
             (goto-char (point-min))
             (cl-loop for i from 1 to (1- current-headline-indent)
                do (org-demote-subtree))))
         (org-overview)
         (goto-char initial-point)
         (org-align-tags t)
         (save-buffer)
         (message (org-glance:format "Headline \"${current-headline-title}\" is now up to date"))
         t))
    ))

(cl-defun org-glance-overview:comment ()
  "Toggle comment headline at point."
  (interactive)
  (save-window-excursion
    (->> (org-glance-headline:at-point)
      (org-glance-headline:id)
      (org-glance-metastore:get-headline)
      (org-glance-headline:visit))
    (org-toggle-comment)
    (save-buffer))
  (org-glance-overview:pull))

(cl-defun org-glance-overview:archive ()
  "Toggle archive headline at point."
  (interactive)
  (save-window-excursion
    (->> (org-glance-headline:at-point)
      (org-glance-headline:id)
      (org-glance-metastore:get-headline)
      (org-glance-headline:visit))
    (org-toggle-archive-tag)
    (save-buffer))
  (org-glance-overview:pull))

;; (cl-defun org-glance-overview:edit-mode ()
;;   (interactive)
;;   (org-glance-overview:for-all
;;       (error "not implemented yet")
;;     (let* ((headline (org-glance-headline:at-point))
;;            (beg (org-element-property :begin (org-glance-headline:at-point)))
;;            (end (org-element-property :end (org-glance-headline:at-point))))
;;       (hlt-unhighlight-region beg end)
;;       ;; (hlt-highlight-region beg end 'expal-block-hover-face)
;;       (remove-text-properties beg end '(read-only t)))))

(cl-defun org-glance-overview:original-headline ()
  (org-glance-metastore:get-headline (org-glance-headline:id)))

(cl-defun org-glance-overview:add-relation
    (&optional
       (source (org-glance-overview:original-headline))
       (target (condition-case choice
                   (org-glance-metastore:choose-headline)
                 (org-glance-exception:headline-not-found
                  (org-glance-overview:capture
                   (org-glance-view:choose "Unknown thing. Please, specify it's class to capture: ")
                   (cadr choice))))))
  "In `org-glance-overview-mode' add relation from original headline at point SOURCE to TARGET."
  (interactive)
  (org-glance-headline:add-biconnected-relation source target)
  (org-glance-overview:pull))

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

(org-glance:provide)
