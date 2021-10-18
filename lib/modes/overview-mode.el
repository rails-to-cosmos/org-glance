(require 'org-glance-module)

(org-glance:require
  highlight
  org-attach
  org-capture)

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

(cl-defun org-glance-overview:choose-headline-and-jump ()
  "Choose `org-glance-headline' from current overview buffer and goto it."
  (let ((headlines (org-glance-headline:extract (current-buffer))))
    (org-glance-headline:search-buffer-by-id
     (org-glance-headline:id
      (org-glance-scope--choose-headline
       (org-completing-read "Specify headline: "
                            (mapcar #'org-glance-headline:title (--filter (org-glance-headline:active? it) headlines)))
       headlines)))))

(cl-defmacro org-glance-overview:for-each (&rest forms)
  "Eval FORMS on headline at point.
If point is before first heading, eval forms on each headline."
  (declare (indent 0) (debug t))
  `(org-glance:interactive-lambda
     (if (org-before-first-heading-p)
         (when (or (not current-prefix-arg)
                   (y-or-n-p "Apply action to all headlines in buffer?"))
           (goto-char (point-min))
           (while (and (org-glance-headline:search-forward) (sit-for 0))
             (when (= (org-glance-headline:level) 1)
               ,@forms)))
       ,@forms)))

(cl-defmacro org-glance-overview:for-one (&rest forms)
  "Eval FORMS on headline at point.
If point is before first heading, prompt for headline and eval forms on it."
  (declare (indent 0) (debug t))
  `(org-glance:interactive-lambda
     (when (org-before-first-heading-p)
       (org-glance-overview:choose-headline-and-jump))
     ,@forms))

;; lightweight methods applied for current headline
(define-key org-glance-overview-mode-map (kbd ";") #'org-glance-overview:archive)
(define-key org-glance-overview-mode-map (kbd "#") #'org-glance-overview:comment)
(define-key org-glance-overview-mode-map (kbd "<") #'beginning-of-buffer)
(define-key org-glance-overview-mode-map (kbd ">") #'end-of-buffer)
(define-key org-glance-overview-mode-map (kbd "^")
  (org-glance:interactive-lambda
    (save-excursion
      (let ((inhibit-read-only t))
        (goto-char (point-min))
        (org-glance-overview:sort)
        (org-overview)
        (save-buffer)))))

(define-key org-glance-overview-mode-map (kbd "@")
  (org-glance-overview:for-one
    (org-glance-overview:add-relation)))

(define-key org-glance-overview-mode-map (kbd "RET")
  (org-glance-overview:for-one
    (org-glance-overview:materialize-headline)))

(define-key org-glance-overview-mode-map (kbd "/")
  (org-glance:interactive-lambda
    (org-glance-overview:choose-headline-and-jump)))

(define-key org-glance-overview-mode-map (kbd "F")
  (org-glance-overview:for-one
    (org-attach-reveal-in-emacs)))

(define-key org-glance-overview-mode-map (kbd "!")
  (org-glance-overview:for-each
    (org-glance-overview:doctor)))

(define-key org-glance-overview-mode-map (kbd "g")
  (org-glance-overview:for-each
    (org-glance-overview:pull)))

(define-key org-glance-overview-mode-map (kbd "v")
  (org-glance-overview:for-one
    (org-glance-overview:visit-headline)))

(define-key org-glance-overview-mode-map (kbd "a") #'org-glance-overview:agenda)
(define-key org-glance-overview-mode-map (kbd "n") #'org-glance-headline:search-forward)
(define-key org-glance-overview-mode-map (kbd "p") #'org-glance-headline:search-backward)
(define-key org-glance-overview-mode-map (kbd "q") #'bury-buffer)
(define-key org-glance-overview-mode-map (kbd "d")
  (org-glance:interactive-lambda
    (let* ((origins (cl-loop
                       for headline in (org-glance-headline:extract (current-buffer))
                       collect (->> headline
                                    org-glance-headline:id
                                    org-glance-metastore:get-headline
                                    org-glance-headline:file)))
           (scope (seq-uniq origins #'string=)))
      (org-drill scope))))

(define-key org-glance-overview-mode-map (kbd "k")
  (org-glance-overview:for-one
    (org-glance-overview:kill-headline)))

;; (define-key org-glance-overview-mode-map (kbd "r") #'org-glance-overview:move-headline)
(define-key org-glance-overview-mode-map (kbd "z") #'org-glance-overview:vizualize)

(define-key org-glance-overview-mode-map (kbd "C-c C-p") #'org-glance-edit-mode:start)

(define-key org-glance-overview-mode-map (kbd "+")
  (org-glance:interactive-lambda
    (org-glance-overview:capture
     :class (org-glance-overview:class))))

(define-key org-glance-overview-mode-map (kbd "*") #'org-glance-overview:import-headlines)

(cl-defun org-glance-overview:register-headline-in-metastore (headline class)
  (let* ((metastore-location (-some->> class
                               org-glance:get-class
                               org-glance-view:metastore-location))
         (metastore (org-glance-metastore:read metastore-location)))
    (org-glance-metastore:add-headline headline metastore)
    (org-glance-metastore:write metastore-location metastore)))

(cl-defun org-glance-overview:remove-headline-from-metastore (headline class)
  (let* ((metastore-location (-some->> class
                               org-glance:get-class
                               org-glance-view:metastore-location))
         (metastore (org-glance-metastore:read metastore-location)))
    (org-glance-metastore:rem-headline headline metastore)
    (org-glance-metastore:write metastore-location metastore)))

(cl-defun org-glance-overview:register-headline-in-overview (headline class)
  "Add HEADLINE clone in overview VIEW-ID file."
  (save-window-excursion
    (org-glance-overview class)
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

(cl-defun org-glance-overview:remove-headline-from-overview (headline class)
  "Add HEADLINE clone in overview VIEW-ID file."
  (save-window-excursion
    (org-glance-overview class)
    (save-excursion
      (org-glance-headline:search-buffer-by-id (org-glance-headline:id headline))
      (let ((inhibit-read-only t))
        (kill-region (org-entry-beginning-position) (org-entry-end-position))
        (save-buffer)))))

(cl-defun org-glance-overview:register-headline-in-write-ahead-log (headline class)
  (org-glance-headline:with-materialized-headline headline
    (let ((id (intern (org-glance-headline:id headline)))
          (class (if (symbolp class) class (intern class))))
      (org-glance-posit:write
       (org-glance-posit (list class 'is-class))
       (org-glance-posit (list id 'thing) (list class 'class))
       (org-glance-posit (list id 'origin) :value (list (org-glance-headline:file headline) (org-glance-headline:begin headline)))
       (org-glance-posit (list id 'title) :value (org-glance-headline:title headline))
       (org-glance-posit (list id 'contents)
                         :value (save-excursion
                                  (org-end-of-meta-data t)
                                  ""
                                  ;; (base64-encode-string
                                  ;;  (buffer-substring-no-properties (point) (point-max))
                                  ;;  t)
                                  ))
       (org-glance-posit (list id 'extractable)
                         :value (save-excursion
                                  (org-end-of-meta-data t)
                                  (when (re-search-forward org-glance:key-value-pair-re nil t)
                                    t)))
       (org-glance-posit (list id 'openable)
                         :value (save-excursion
                                  (org-end-of-meta-data t)
                                  (when (re-search-forward org-any-link-re nil t)
                                    t)))
       (org-glance-posit (list id 'decryptable)
                         :value (save-excursion
                                  (org-end-of-meta-data t)
                                  (looking-at "aes-encrypted V [0-9]+.[0-9]+-.+\n")))))))

(cl-defun org-glance:capture-headline-at-point
    (&optional (view-id (org-completing-read "Capture headline for view: " (org-glance-view:ids)))
     &key (remove-original t))
  (interactive)
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
    (&key
       (class (org-glance:choose-class))
       (file (make-temp-file "org-glance-" nil ".org"))
       (default "")
       (callback nil))
  (interactive)
  (org-glance:log-debug "User input: %s" default)
  (find-file file)
  (setq-local
   org-glance-capture:id (format "%s-%s-%s"
                                 class
                                 system-name
                                 (s-join "-" (mapcar #'number-to-string (current-time))))
   org-glance-capture:class (if (symbolp class) class (intern class))
   org-glance-capture:default default)

  (add-hook 'org-capture-prepare-finalize-hook 'org-glance-capture:prepare-finalize-hook 0 t)
  (add-hook 'org-capture-after-finalize-hook 'org-glance-capture:after-finalize-hook 0 t)
  (when callback (add-hook 'org-capture-after-finalize-hook callback 1 t))

  (let ((org-capture-templates (list (list "_" "Thing" 'entry (list 'file file) "* TODO %?"))))
    (org-capture nil "_")))

(cl-defun org-glance-capture:prepare-finalize-hook ()
  "Preprocess headline before capturing.

Buffer local variables: `org-glance-capture:id', `org-glance-capture:class', `org-glance-capture:default'."
  (goto-char (point-min))
  (or (org-at-heading-p) (org-next-visible-heading 0))
  (org-set-property "ORG_GLANCE_ID" org-glance-capture:id)
  (org-toggle-tag (format "%s" org-glance-capture:class) t))

(cl-defun org-glance-capture:after-finalize-hook ()
  "Register captured headline in metastore.

Buffer local variables: `org-glance-capture:id', `org-glance-capture:class', `org-glance-capture:default'."

  (org-glance:log-debug
   "Finalize capture (id: %s, class: %s)"
   org-glance-capture:id
   org-glance-capture:class)

  (unless (org-glance:get-class org-glance-capture:class)
    (save-window-excursion
      (org-glance:create-class org-glance-capture:class)))

  (let* ((id org-glance-capture:id)
         (class org-glance-capture:class)
         (headline (org-glance-headline:search-buffer-by-id id))
         (refile-dir (make-temp-file
                      (-org-glance:make-file-directory
                       (f-join (org-glance-view:resource-location class)
                               (concat (format-time-string "%Y-%m-%d_")
                                       (->> (org-glance-headline:title headline)
                                            (replace-regexp-in-string "[^a-z0-9A-Z_]" "-")
                                            (replace-regexp-in-string "\\-+" "-")
                                            (replace-regexp-in-string "\\-+$" "")
                                            (s-truncate 30))
                                       "-")))
                      'directory))
         (tmp-file (org-glance-headline:file headline))
         (new-file (-org-glance:make-file-directory (f-join refile-dir (format "%s.org" class)))))
    (org-glance:log-debug "Generate headline directory: %s" refile-dir)
    (org-set-property "DIR" (abbreviate-file-name refile-dir))
    (save-buffer)
    (kill-buffer)

    (f-move tmp-file new-file)
    (org-glance-headline:enrich headline :file new-file)

    (org-glance-overview class)

    (org-glance:log-debug "Register headline of class %s in metastore: %s"
                          (pp-to-string class)
                          (pp-to-string headline))

    (org-glance-overview:register-headline-in-metastore headline class)
    (org-glance-overview:register-headline-in-overview headline class)
    (org-glance-overview:register-headline-in-write-ahead-log headline class)

    (org-overview)
    (org-glance-headline:search-buffer-by-id id)))

(cl-defun org-glance-overview:import-headlines
    (path
     &optional
       (class (org-glance-view:completing-read)))
  (interactive "fImport from location: ")
  (when (y-or-n-p (org-glance:format "Import headlines of class ${class} from ${path}?"))
    (cl-loop
       for file in (org-glance-scope path)
       do
         (org-glance:log-info "Scan file %s" file)
         (redisplay)
         (cl-loop
            for headline in (org-glance-headline:extract file)
            when (-contains?
                  (mapcar #'downcase (org-element-property :tags headline))
                  (downcase (symbol-name class)))
            do
              (org-glance-overview:register-headline-in-metastore headline class)
              (org-glance-overview:register-headline-in-overview headline class)
            ;; (org-glance-overview:register-headline-in-write-ahead-log el class)
              ))))

(define-minor-mode org-glance-overview-mode
    "A minor read-only mode to use in overview files."
  nil nil org-glance-overview-mode-map
  (read-only-mode 'toggle))

(defvar org-glance-edit-mode-map (make-sparse-keymap)
  "Edit entries in `org-glance-edit-mode'.")

(define-key org-glance-edit-mode-map (kbd "C-c C-c") 'org-glance-edit-mode:apply)

(define-minor-mode org-glance-edit-mode
    "A minor mode to edit and sync overview files."
  nil nil org-glance-edit-mode-map)

(cl-defun org-glance-edit-mode:start ()
  (interactive)
  (org-glance-edit-mode +1)
  (org-glance-overview-mode -1)
  (org-glance:log-info "Edit mode is now enabled."))

(cl-defun org-glance-edit-mode:apply ()
  (interactive)
  (org-glance-edit-mode -1)
  (org-glance-overview-mode +1)
  (org-glance:log-info "All changes have been applied."))

(cl-defun org-glance-overview:directory (&optional (view-id (org-glance-view:completing-read)))
  "Path to file where VIEW-ID headlines are stored."
  (let ((view-name (s-downcase (format "%s" view-id))))
    (abbreviate-file-name
     (f-join org-glance-directory view-name))))

(cl-defun org-glance-overview:location (&optional (view-id (org-glance-view:completing-read)))
  "Path to file where VIEW-ID headlines are stored."
  (when view-id
    (let ((view-name (s-downcase (format "%s" view-id))))
      (f-join org-glance-directory view-name (concat view-name ".org")))))

(cl-defun org-glance-headline:main-role (&optional (headline (org-glance-overview:original-headline)))
  "Assume main role of HEADLINE as role directory where it is stored."
  (cl-loop
     for view-id in (org-glance-headline:tags headline)
     for overview-directory = (org-glance-overview:directory view-id)
     for original-directory = (org-glance-headline:file headline)
     for common-parent = (abbreviate-file-name (f-common-parent (list overview-directory original-directory)))
     when (f-equal? common-parent overview-directory)
     do (return view-id)))

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

(cl-defun org-glance-overview:create (&optional (class (org-glance-view:completing-read)))
  (interactive)
  (let* ((inhibit-read-only t)
         (filename (-org-glance:make-file-directory
                    (org-glance-overview:location class)))
         (header (let ((category class))
                   (org-glance:format org-glance-overview:header))))
    (with-temp-file filename
      (org-mode)
      (insert header))
    (find-file filename)))

(cl-defun org-glance-overview (&optional (class (org-glance-view:completing-read)))
  (interactive)
  (when class
    (when-let (location (org-glance-overview:location class))
      (if (file-exists-p location)
          (find-file location)
        (org-glance-overview:create class)))))

(cl-defun org-glance-overview:agenda ()
  (interactive)
  (let ((org-agenda-files (list (org-glance-overview:for-all
                                    (buffer-file-name)
                                  (org-glance-headline:file (org-glance-overview:original-headline))))))
    (org-agenda-list)
    (org-agenda-fortnight-view)
    (switch-to-buffer org-agenda-buffer)
    (delete-other-windows)))

(cl-defun org-glance-overview:agenda* ()
  (interactive)
  (let ((org-agenda-files (mapcar 'org-glance-overview:location (org-glance-view:ids))))
    (org-agenda-list)
    (org-agenda-fortnight-view)
    (switch-to-buffer org-agenda-buffer)
    (delete-other-windows)))

(cl-defun org-glance-overview:materialize-headline ()
  (interactive)
  (let* ((headline (org-glance-overview:original-headline))
         (buffer (org-glance-headline:materialized-buffer headline)))
    (if (buffer-live-p buffer)
        (switch-to-buffer buffer)
      (org-glance-headline:materialize headline))))

(cl-defun org-glance-overview:visit-headline ()
  (interactive)
  (org-glance-overview:for-all
      nil
    (let ((offset (- (point) (save-excursion
                               (org-glance-headline:search-parents)
                               (point)))))
      (-some->> (org-glance-headline:at-point)
        org-glance-headline:id
        org-glance-metastore:get-headline
        org-glance-headline:visit)
      (forward-char offset))))

(cl-defun org-glance-overview:class ()
  (save-excursion
    (goto-char (point-min))
    (intern (downcase (org-get-category)))))

(defmacro org-glance-doctor:when (predicate prompt &rest forms)
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
  ;; - [x] fix non-relative DIR properties

  (when (org-glance-overview:pull)
    (let* ((view-id (org-glance-overview:class))
           (original-headline (org-glance-overview:original-headline))
           (original-headline-location (org-glance-headline:file original-headline))
           (dir (org-element-property :DIR original-headline))
           (archive (org-element-property :ARCHIVE original-headline))
           (main-role (org-glance-headline:main-role))
           (title (org-glance-headline:title))
           (raw-value (org-glance-headline:raw-value original-headline))
           (now (format-time-string (org-time-stamp-format 'long 'inactive) (current-time))))

      (org-glance-doctor:when (and dir (not (string= dir (abbreviate-file-name dir))))
          "Headline \"${title}\" contains full path in DIR property. Abbreviate it?"
        (org-glance-headline:with-materialized-headline original-headline
          (org-set-property "DIR" (abbreviate-file-name dir))))

      (org-glance-doctor:when (and archive (not (string= archive (abbreviate-file-name archive))))
          "Headline \"${title}\" contains full path in ARCHIVE property. Abbreviate it?"
        (org-glance-headline:with-materialized-headline original-headline
          (org-set-property "ARCHIVE" (abbreviate-file-name archive))))

      (org-glance-doctor:when (null main-role)
          "Headline \"${title}\" is located outside of ${view-id} directory: ${original-headline-location}. Capture it?"
        (let ((captured-headline (org-glance-headline:with-materialized-headline original-headline
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
  (let ((class (org-glance-overview:class)))
    (when (y-or-n-p (org-glance:format "Rebuild ${class}?"))
      (save-buffer)
      (kill-buffer)
      (org-glance-metastore:create (org-glance-view:metastore-location (org-glance:get-class class)))
      (org-glance-overview:create class)
      (org-glance-overview:import-headlines org-glance-directory class)
      (org-glance:log-info (org-glance:format "View ${class} is now up to date")))))

(cl-defun org-glance-overview:pull* ()
  "Apply `org-glance-overview:pull' to each headline in current overview file."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (and (org-glance-headline:search-forward))
      (org-glance-overview:pull))))

(cl-defun org-glance-overview:kill-headline (&key (force nil))
  "Remove `org-glance-headline' from overview, don't ask to confirm if FORCE is t."
  (interactive)
  (org-glance-headline:search-parents)
  (let ((title (org-glance-headline:title))
        (class (org-glance-overview:class))
        (original-headline (org-glance-overview:original-headline)))
    (when (or force (y-or-n-p (org-glance:format "Revoke the class \"${class}\" from \"${title}\"?")))
      (org-glance-headline:with-materialized-headline original-headline
        (org-toggle-tag (format "%s" class) 'off)))))

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
    (cond
      ((null original-headline-contents)
       (if (y-or-n-p (org-glance:format "Original headline for \"${current-headline-title}\" not found. Remove it from overview?"))
           (org-glance-overview:kill-headline :force t)
         (org-glance-exception:HEADLINE-NOT-FOUND "Original headline not found"))
       nil)
      ((string= current-headline-contents original-headline-contents)
       (org-glance:log-info (org-glance:format "Headline \"${current-headline-title}\" is up to date"))
       t)
      (t (save-excursion
           (save-restriction
             (org-glance-headline:search-parents)
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
         (org-glance:log-info (org-glance:format "Headline \"${current-headline-title}\" is now up to date"))
         t))))

(cl-defun org-glance-overview:comment ()
  "Comment headline at point."
  (interactive)
  (save-window-excursion
    (->> (org-glance-headline:at-point)
      (org-glance-headline:id)
      (org-glance-metastore:get-headline)
      (org-glance-headline:visit))
    (org-toggle-comment)
    (save-buffer))
  (org-glance-overview:pull)
  (org-glance-headline:search-forward))

(cl-defun org-glance-overview:archive ()
  "Archive headline at point."
  (interactive)
  (save-window-excursion
    (->> (org-glance-headline:at-point)
      (org-glance-headline:id)
      (org-glance-metastore:get-headline)
      (org-glance-headline:visit))
    (org-toggle-archive-tag)
    (save-buffer))
  (org-glance-overview:pull)
  (org-glance-headline:search-forward))

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
  (save-window-excursion
    (save-excursion
      (->> (org-glance-headline:at-point)
           org-glance-headline:id
           org-glance-metastore:get-headline
           org-glance-headline:visit)
      (org-glance-headline:at-point))))

(cl-defun org-glance-overview:add-relation ()
  "In `org-glance-overview-mode' add relation from original headline at point SOURCE to TARGET."
  (interactive)
  (lexical-let ((source (org-glance-overview:original-headline)))
    (org-glance:ensure-headline-apply
        :action (lambda (headline)
                  (org-glance-headline:add-biconnected-relation source headline)))))

(cl-defun org-glance-overview:vizualize ()
  (interactive)
  (org-glance-overview:for-all
      (error "not implemented yet")
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
