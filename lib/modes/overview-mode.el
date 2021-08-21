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

(define-key org-glance-overview-mode-map (kbd "+") 'org-glance-overview:capture-headline)

(cl-defun org-glance-overview:register-headline (headline view-id)
  "Register HEADLINE in metastore and overview file."
  (let* ((inhibit-read-only t)
         (metastore-location (-some->> view-id
                               org-glance-view:get-view-by-id
                               org-glance-view-metastore-location))
         (metastore (org-glance-metastore:read metastore-location)))

    ;; modify metastore
    (org-glance-metastore:add-headline headline metastore)
    (org-glance-metastore:write metastore-location metastore)

    ;; modify overview
    (find-file (org-glance-overview:location view-id))
    (beginning-of-buffer)
    (org-glance-headline:search-forward)
    (insert (org-glance-headline:contents headline) "\n")
    (beginning-of-buffer)
    (org-glance-overview:sort* '(?o ?p))
    (save-buffer)))

(cl-defun org-glance-overview:capture-headline ()
  (interactive)
  (let* ((view-id (intern (org-get-category)))
         (headline (org-glance-view:capture-headline view-id)))
    (org-glance-overview:register-headline headline view-id)))

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

(cl-defun org-glance-overview:sort* (order &optional group)
  (interactive)
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

  (save-excursion
    (org-save-outline-visibility nil
      (cond ((null order) nil)
            ((null group) (progn
                            (org-sort-entries nil (car order))
                            (org-glance-overview:sort* (cdr order) (car order))))
            (t (let ((group-fn (case group
                                 (?a #'org-glance-headline:title)
                                 (?p #'org-glance-headline:priority)
                                 (?c #'org-glance-headline:creation-time)
                                 (?o #'org-glance-headline:state)
                                 (?t #'org-glance-headline:creation-time)
                                 (t nil)))
                     (comp-fn (case group
                                (?a #'string=)
                                (?p #'eql)
                                (?c #'string=)
                                (?o #'string=)
                                (?t #'string=)
                                (t nil))))
                 (beginning-of-buffer)
                 (org-glance-headline:search-forward)
                 (while (< (point) (point-max))
                   (let* ((group-state (funcall group-fn))
                          (beginning-of-group (point))
                          (end-of-group (cl-loop
                                           while (and (< (point) (point-max))
                                                      (funcall comp-fn group-state (funcall group-fn)))
                                           do (org-glance-headline:search-forward)
                                           finally (return (point)))))
                     (set-mark beginning-of-group)
                     (goto-char end-of-group)
                     (org-sort-entries nil (car order))
                     (goto-char end-of-group)))
                 (org-glance-overview:sort* (cdr order) (car order))))))))

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
      (org-glance-overview:sort* '(?o ?p))
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
  (org-glance-overview:for-all
      (save-excursion
        (goto-char (point-min))
        (while (org-glance-headline:search-forward)
          (when (= (org-glance-headline:level) 1)
            (org-glance-overview:doctor))))
    (save-window-excursion
      (org-glance-overview:pull)
      (let* ((view-id (save-excursion
                        (goto-char (point-min))
                        (intern (org-get-category))))
             (original-headline (org-glance-overview:original-headline))
             (original-headline-location (org-glance-headline:file original-headline))
             (overview-file-name (org-glance-overview:location view-id))
             (overview-location (file-name-directory overview-file-name))
             (common-location (f-common-parent (list overview-location original-headline-location)))
             (title (org-glance-headline:title))
             (now (format-time-string (org-time-stamp-format 'long 'inactive) (current-time))))
        (when (s-matches? org-link-any-re title)
          (when (y-or-n-p (org-glance:format "Headline \"${title}\" contains link in title. Move link to the body of headline?"))
            (org-glance-headline:narrow original-headline
              (org-glance-headline:add-log-note (org-glance:format "- Link captured: ${title} on ${now}"))
              (org-glance-headline:goto-beginning-of-current-headline)
              (org-beginning-of-line)
              (org-kill-line)
              (insert (with-temp-buffer
                        (insert title)
                        (goto-char (point-min))
                        (let* ((link (org-element-link-parser))
                               (contents-begin (org-element-property :contents-begin link))
                               (contents-end (org-element-property :contents-end link)))
                          (if (and contents-begin contents-end)
                              (buffer-substring-no-properties contents-begin contents-end)
                            (let ((webpage-title (org-glance:title-from-url (org-element-property :raw-link link))))
                              (if (string-empty-p webpage-title)
                                  (read-string "New title: ")
                                webpage-title))))))
              (save-buffer))
            (org-glance-overview:pull)))
        (unless (string= overview-location common-location)
          (when (y-or-n-p (org-glance:format "Headline \"${title}\" is located outside of ${view-id} directory: ${original-headline-location}. Capture it?"))
            (org-glance-headline:visit original-headline)
            (let ((captured-headline (org-glance-view:capture-headline-at-point view-id)))
              (org-glance-overview:register-headline captured-headline view-id))))))))

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
         (current-headline-title (org-glance-headline:title current-headline))
         (current-headline-indent (org-glance-headline:level current-headline))
         (current-headline-contents (org-glance-headline:contents current-headline))
         (original-headline (org-glance-overview:original-headline))
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
