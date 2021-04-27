;; org-glance view means a set of headlines in scope of org-files

(require 'org-glance-module)

(require 'org)
(require 'org-element)
(require 'subr-x)

(org-glance-module-import lib.core.scope)
(org-glance-module-import lib.core.serde)

(defvar org-glance-view-mode-map (make-sparse-keymap)
  "Extend `org-mode' map with sync abilities.")

(define-minor-mode org-glance-view-mode
    "A minor mode to be activated only in materialized view editor."
  nil nil org-glance-view-mode-map)

(defvar org-glance-view-default-type '(all)
  "Default type for all views.")

(defvar --org-glance-view-pwd nil)
(defvar --org-glance-view-src nil)
(defvar --org-glance-view-beg nil)
(defvar --org-glance-view-end nil)
(defvar --org-glance-view-hash nil)
(defvar --org-glance-view-indent nil)

(defvar org-glance-views (make-hash-table :test 'equal))
(defvar org-glance-view-actions (make-hash-table :test 'equal))

(defcustom org-glance-view-location (f-join user-emacs-directory "org-glance" "views")
  "The location where view metadata should be stored."
  :group 'org-glance
  :type 'string)

(defcustom org-glance-materialized-view-buffer "*org-glance materialized view*"
  "Default buffer name for materialized view."
  :group 'org-glance
  :type 'string)

(defconst org-glance-view-selector:all '!All)

(define-key org-glance-view-mode-map (kbd "C-x C-s") #'org-glance-view-sync-subtree)
(define-key org-glance-view-mode-map (kbd "C-c C-q") #'kill-current-buffer)

(define-error 'org-glance-source-file-corrupted
    "Source file corrupted, please reread" 'user-error)

(cl-defun org-glance-source-file-corrupted (format &rest args)
  (signal 'org-glance-source-file-corrupted (list (apply #'format-message format args))))

(define-error 'org-glance-properties-corrupted
    "Materialized view properties corrupted, please reread" 'user-error)

(cl-defun org-glance-properties-corrupted (format &rest args)
  (signal 'org-glance-properties-corrupted (list (apply #'format-message format args))))

(defcustom org-glance-after-materialize-hook nil
  "Normal hook that is run after a buffer is materialized in separate buffer."
  :options '(copyright-update time-stamp)
  :type 'hook
  :group 'org-glance)

(defcustom org-glance-after-materialize-sync-hook nil
  "Hook that is run after a materialized buffer is synchronized to its source file."
  :options '(copyright-update time-stamp)
  :type 'hook
  :group 'org-glance)

(defcustom org-glance-before-materialize-sync-hook nil
  "Normal hook that is run before a materialized buffer is synchronized to its source file."
  :options '(copyright-update time-stamp)
  :type 'hook
  :group 'org-glance)

(cl-defstruct (org-glance-view
                ;; (:constructor org-glance-def-view (id
                ;;                                    &key
                ;;                                    (type org-glance-view-default-type)
                ;;                                    (scope nil)))
                )
  id type scope)

(cl-defun org-glance-view-export-filename (&optional (view-id (org-glance-read-view-id)))
  "Path to file where VIEW-ID exported headlines are stored."
  (f-join org-glance-view-location
          (s-downcase (format "%s" view-id))
          (s-downcase (format "%s.org" view-id))))

(defun org-glance-exports ()
  (org-glance--list-files-recursively org-glance-view-location))

(define-error 'org-glance-view-not-modified "No changes made in materialized view" 'user-error)
(cl-defun org-glance-view-not-modified (format &rest args)
  (signal 'org-glance-view-not-modified (list (apply #'format-message format args))))

(defun org-glance-list-view-ids ()
  "List registered views."
  (sort (hash-table-keys org-glance-views) #'s-less?))

(defun org-glance--views-at-point ()
  (-intersection (org-glance--collect-tags)
                 (cl-loop for tag in (org-glance-list-view-ids)
                    collect (downcase (symbol-name tag)))))

(defun org-glance--back-to-view-heading ()
  (org-glance--back-to-heading)
  (or (org-glance--views-at-point)
      (progn
        (org-up-element)
        (org-glance--back-to-view-heading))))

(defun org-glance--nearest-view-heading ()
  (save-excursion
    (org-glance--back-to-view-heading)))

(defun org-glance-view:specify-attach-directory ()
  "Specify dir and archive paths for current headline."
  (interactive)
  (let* ((view (car (org-glance--nearest-view-heading)))
         (path (read-directory-name (format "Specify directory for %s: " view)
                                    (f-join org-glance-view-location view)
                                    nil nil "")))
    (org-glance--ensure-path path)
    (org-set-property "DIR" path)
    (org-set-property "ARCHIVE" (f-join path (format "%s.org::" view)))
    (org-set-property "COOKIE_DATA" "todo recursive")))

(cl-defmethod org-glance-view ((view-id symbol)) (gethash view-id org-glance-views))
(cl-defmethod org-glance-view ((view-id string)) (org-glance-view (intern view-id)))

(cl-defmethod org-glance-view-db ((view org-glance-view))
  (let ((view-id (downcase (symbol-name (org-glance-view-id view)))))
    (f-join org-glance-view-location
            view-id
            (format "%s.el" view-id))))

(cl-defmethod org-glance-view-filter ((view org-glance-view))
  (-partial
   #'(lambda (view headline)
       (-contains?
        (mapcar #'downcase (org-element-property :tags headline))
        (downcase (symbol-name (org-glance-view-id view)))))
   view))

(cl-defun org-glance-view-reread (&optional
                                    (view-id (org-glance-read-view-id)))
  (interactive)
  (message "Reread view %s" view-id)
  (let* ((view (gethash view-id org-glance-views))
         (db (org-glance-view-db view))
         (filter (org-glance-view-filter view))
         (scope (or (org-glance-view-scope view) org-glance-default-scope)))
    (org-glance-db-init db (org-glance-scope-headlines scope filter))
    view))

(defun org-glance-view-headlines (view)
  "List headlines as org-elements for VIEW."
  (org-glance-headlines
   :db (org-glance-view-db view)
   :scope (or (org-glance-view-scope view) org-glance-default-scope)
   :filter (org-glance-view-filter view)))

(cl-defmethod org-glance-view-headlines/formatted ((view org-glance-view))
  "List headlines as formatted strings for VIEW."
  (->> view
    org-glance-view-headlines
    (mapcar #'org-glance--format-headline)
    (mapcar #'(lambda (hl) (format "[%s] %s" (org-glance-view-id view) hl)))))

(cl-defgeneric org-glance-view-prompt (view action)
  "Generate prompt for VIEW. Assume ACTION context.")

(cl-defmethod org-glance-view-prompt ((view org-glance-view) (action symbol))
  (s-titleize (format "%s %s: " action (org-glance-view-id view))))

(cl-defgeneric org-glance-view-action-resolve (view action))

(cl-defmethod org-glance-view-action-resolve ((view org-glance-view) (action symbol))
  (let* ((action-types (->> org-glance-view-actions
                         (gethash action)
                         (-sort (lambda (lhs rhs) (> (length lhs) (length rhs))))))
         (view-actions (cl-loop for action-type in action-types
                          with view-type = (org-glance-view-type view)
                          when (cl-subsetp action-type view-type)
                          return action-type)))
    (or view-actions
        (car (member org-glance-view-default-type (gethash action org-glance-view-actions))))))

(cl-defmethod org-glance-view-action-resolve ((view null) (action symbol))
  (user-error "Assertion error: unable to resolve action when view is null"))

(defun org-glance-view-sync-subtree ()
  (interactive)
  (save-excursion
    (cl-loop while (org-up-heading-safe))
    (let* ((source --org-glance-view-src)
           (beg --org-glance-view-beg)
           (end --org-glance-view-end)
           (promote-level --org-glance-view-indent)
           (glance-hash --org-glance-view-hash)
           (mat-hash (org-glance-view-subtree-hash))
           (src-hash (org-glance-view-source-hash)))

      (unless (string= glance-hash src-hash)
        (org-glance-source-file-corrupted source))

      (when (string= glance-hash mat-hash)
        (org-glance-view-not-modified source))

      (when t ;; (y-or-n-p "Subtree has been modified. Apply changes?")
        (with-demoted-errors (run-hooks 'org-glance-before-materialize-sync-hook))

        (let ((new-contents
               (save-restriction
                 (org-narrow-to-subtree)
                 (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
                   (with-temp-buffer
                     (org-mode)
                     (insert buffer-contents)
                     (goto-char (point-min))
                     (org-glance--demote-subtree promote-level)
                     (buffer-substring-no-properties (point-min) (point-max)))))))

          (with-temp-file source
            (org-mode)
            (insert-file-contents source)
            (delete-region beg end)
            (goto-char beg)
            (insert new-contents)
            (setq end (point)))

          (setq-local --org-glance-view-beg beg)
          (setq-local --org-glance-view-end end)
          (setq-local --org-glance-view-hash (org-glance-view-source-hash))

          (with-demoted-errors (run-hooks 'org-glance-after-materialize-sync-hook)))))))

(defun org-glance-view-subtree-hash ()
  (save-restriction
    (org-narrow-to-subtree)
    (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
      (with-temp-buffer
        (org-mode)
        (insert buffer-contents)
        (goto-char (point-min))
        (org-glance--promote-subtree)
        (buffer-hash)))))

(defun org-glance-view-source-hash ()
  (let ((src --org-glance-view-src)
        (beg --org-glance-view-beg)
        (end --org-glance-view-end))
    (with-temp-buffer
      (insert-file-contents src)
      (let ((subtree (condition-case nil
                         (buffer-substring-no-properties beg end)
                       (error (org-glance-properties-corrupted "Materialized properties corrupted, please reread")))))
        (with-temp-buffer
          (org-mode)
          (insert (s-trim subtree))
          (cl-loop while (org-up-heading-safe))
          (org-glance--promote-subtree)
          (buffer-hash))))))

(cl-defmethod org-glance-view-delete ((view-id symbol))
  (remhash view-id org-glance-views))

(defun org-glance-capture-subtree-at-point ()
  (interactive)
  (unless (org-at-heading-p) (org-back-to-heading))
  ;; (let* ((other-views (seq-difference
  ;;                      (org-glance-list-view-ids)
  ;;                      (mapcar #'intern (org-get-tags))))
  ;;        (view-id (org-completing-read "View: " other-views))
  ;;        (view (org-glance-view view-id)))
  ;;   (org-toggle-tag view-id)
  ;;   ;; (loop for type in (org-glance-view-type view)
  ;;   ;;       do (pp type))
  ;;   )
  )

(cl-defun org-glance-read-view-id (&optional (prompt "Choose view: "))
  "Run completing read PROMPT on registered views filtered by TYPE."
  (let ((views (org-glance-list-view-ids)))
    (if (> (length views) 1)
        (let ((view (org-completing-read prompt (append (list org-glance-view-selector:all) views))))
          (intern view))
      (car views))))

(cl-defun org-glance-read-view (&optional (prompt "Choose view: "))
  "Run completing read PROMPT on registered views filtered by TYPE."
  (gethash (org-glance-read-view-id prompt) org-glance-views))

(cl-defun org-glance-view-agenda (&optional
                                    (view-id (org-glance-read-view-id)))
  (interactive)
  (let ((org-agenda-files
         (cond ((string= view-id org-glance-view-selector:all)
                (cl-loop for view in (org-glance-list-view-ids)
                   collect (org-glance-view-export-filename view)))
               (t (list (org-glance-view-export-filename view-id))))))
    (org-agenda-list)))

(cl-defun org-glance-view-visit
    (&optional
       (view-id (org-glance-read-view-id)))
  (interactive)
  (find-file (org-glance-view-export-filename view-id)))

(cl-defun org-glance-def-view (id &key type scope)
  (unless (eq nil (gethash id org-glance-views))
    (user-error "View %s is already registered." id))
  (let ((view (make-org-glance-view :id id)))
    (when scope (setf (org-glance-view-scope view) scope))
    (when type  (setf (org-glance-view-type view) type))
    (puthash id view org-glance-views)
    (unless scope
      (message "Default scope is: %s" org-glance-default-scope))
    (message "View \"%s\"%s is now ready to glance %s"
             id
             (if type (concat " of type \"" (s-trim (pp-to-string type)) "\"") "")
             (if scope (concat " over scope \"" (s-trim (pp-to-string scope)) "\"") ""))
    view))

(org-glance-module-provide)
