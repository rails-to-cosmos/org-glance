;; org-glance view means a set of headlines in scope of org-files

(require 'org-glance-module)

(require 'org)
(require 'org-element)
(require 'subr-x)

(org-glance-module-import lib.core.scope)
(org-glance-module-import lib.core.serde)
(org-glance-module-import lib.core.exceptions)

(defvar org-glance-view-mode-map (make-sparse-keymap)
  "Extend `org-mode' map with sync abilities.")

(define-minor-mode org-glance-view-mode
    "A minor mode to be activated only in materialized view editor."
  nil nil org-glance-view-mode-map)

(defvar org-glance-view-default-type '(all)
  "Default type for all views.")

(defvar org-glance-view-summary-header-template "#    -*- mode: org; mode: org-glance-view -*-

#+CATEGORY: {:category}
#+STARTUP: overview

")

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

(define-key org-glance-view-mode-map (kbd "C-x C-s") #'org-glance-view-sync-subtree)
(define-key org-glance-view-mode-map (kbd "C-c C-q") #'kill-current-buffer)

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

(cl-defun org-glance-view-resource-location (&optional (view-id (org-glance-view:completing-read)))
  "Path to directory where VIEW-ID resources and metadata are stored."
  (f-join org-glance-view-location
          (s-downcase (format "%s" view-id))
          "resources"))

(cl-defun org-glance-view:summary-location (&optional (view-id (org-glance-view:completing-read)))
  "Path to file where VIEW-ID exported headlines are stored."
  (let ((view-name (s-downcase (format "%s" view-id))))
    (f-join org-glance-view-location
            view-name
            (format "%s.org_summary" view-name))))

(defun org-glance-exports ()
  (org-glance--list-files-recursively org-glance-view-location))

(define-error 'org-glance-view-not-modified "No changes made in materialized view" 'user-error)
(cl-defun org-glance-view-not-modified (format &rest args)
  (signal 'org-glance-view-not-modified (list (apply #'format-message format args))))

(defun org-glance-view:list-view-ids ()
  "List registered views."
  (sort (hash-table-keys org-glance-views) #'s-less?))

(defun org-glance-view:headline-at-point-view-ids ()
  (-intersection (org-glance--collect-tags)
                 (cl-loop for tag in (org-glance-view:list-view-ids)
                    collect (downcase (symbol-name tag)))))

(defun org-glance-view:back-to-heading ()
  (org-glance-headline:back-to-heading)
  (or (org-glance-view:headline-at-point-view-ids)
      (progn
        (org-up-element)
        (org-glance-view:back-to-heading))))

(defun org-glance-view:nearest-heading ()
  (save-excursion
    (org-glance-view:back-to-heading)))

(defun org-glance-view:specify-attach-directory ()
  "Specify dir and archive paths for current headline."
  (interactive)
  (let* ((view (car (org-glance-view:nearest-heading)))
         (path (read-directory-name (format "Specify directory for %s: " view)
                                    (f-join org-glance-view-location view)
                                    nil nil "")))
    (org-glance--ensure-path path)
    (org-set-property "DIR" path)
    (org-set-property "ARCHIVE" (f-join path (format "%s.org::" view)))
    (org-set-property "COOKIE_DATA" "todo recursive")))

(cl-defmethod org-glance-view ((view-id symbol)) (gethash view-id org-glance-views))
(cl-defmethod org-glance-view ((view-id string)) (org-glance-view (intern view-id)))

(cl-defmethod org-glance-view-metadata-location ((view org-glance-view))
  (let ((view-id (downcase (symbol-name (org-glance-view-id view)))))
    (f-join org-glance-view-location
            view-id
            (format "%s.metadata.el" view-id))))

(cl-defmethod org-glance-view-filter ((view org-glance-view))
  (-partial
   #'(lambda (view headline)
       (-contains?
        (mapcar #'downcase (org-element-property :tags headline))
        (downcase (symbol-name (org-glance-view-id view)))))
   view))

(cl-defun org-glance-view:reread (&optional
                                    (view-id (org-glance-view:completing-read)))
  (interactive)
  (message "Reread view %s" view-id)
  (let* ((view (org-glance-view:get-view-by-id view-id))
         (db (org-glance-view-metadata-location view))
         (filter (org-glance-view-filter view))
         (scope (or (org-glance-view-scope view) org-glance-default-scope)))
    (org-glance-db-init db (org-glance-scope-headlines scope filter))
    view))

(cl-defun org-glance-view:update (&optional (view-id (org-glance-view:completing-read)))
  (interactive)
  (let ((summary-orig-file (org-glance-view:summary-location view-id))
        (summary-temp-file (make-temp-file "org-glance-view-summary-"))
        (file-offsets (make-hash-table :test 'equal))
        (headlines (->> view-id
                     org-glance-view:reread
                     org-glance-view:headlines)))

    (append-to-file (org-glance-expand-template
                     org-glance-view-summary-header-template
                     `(:category ,view-id))
                    nil summary-temp-file)

    (cl-loop for headline in headlines
       do (org-glance-with-headline-materialized headline
            (append-to-file (concat (buffer-substring-no-properties (point-min) (point-max))
                                    "\n") nil summary-temp-file)))

    (progn ;; sort headlines by TODO order
      (find-file summary-temp-file)
      (goto-char (point-min))
      (set-mark (point-max))
      (condition-case nil
          (org-sort-entries nil ?o)
        (error 'nil))
      (org-overview)
      (org-align-tags t)
      (save-buffer)
      (kill-buffer))

    ;; apply changes to original file
    (org-glance--make-file-directory summary-orig-file)
    (when (file-exists-p summary-orig-file)
      (delete-file summary-orig-file t))
    (rename-file summary-temp-file summary-orig-file)

    summary-orig-file))

(cl-defun org-glance-view:patch (&optional (view-id (org-glance-view:completing-read)))
  (interactive)
  (message "Patch view %s" view-id)

  (let* ((view (org-glance-view:get-view-by-id view-id))
         (db (org-glance-view-metadata-location view))
         (filter (org-glance-view-filter view))
         (scope (or (org-glance-view-scope view) org-glance-default-scope))
         (modified 0))
    (cl-loop
       for file in (org-glance-scope scope)
       when (member (file-name-extension file) org-glance-org-scope-extensions)
       do (save-window-excursion
            (find-file file)
            (org-element-map (org-element-parse-buffer 'headline) 'headline
              (lambda (headline)
                (when (and (not (org-glance-headline-p headline))
                           (org-glance-headline:filter filter headline))
                  (goto-char (org-element-property :begin headline))
                  (org-glance-capture-subtree-at-point view-id)
                  (message "Patch file %s headline %s" file headline)
                  (cl-incf modified))))))
    (message "%d files successfully modified" modified)))

(cl-defgeneric org-glance-view:headlines (view))

(cl-defmethod org-glance-view:headlines ((view symbol))
  (org-glance-view:headlines (org-glance-view:get-view-by-id view)))

(cl-defmethod org-glance-view:headlines ((view string))
  (org-glance-view:headlines (org-glance-view:get-view-by-id (intern view))))

(cl-defmethod org-glance-view:headlines ((view org-glance-view))
  "List headlines as org-elements for VIEW."
  (org-glance-headlines
   :db (org-glance-view-metadata-location view)
   :scope (or (org-glance-view-scope view) org-glance-default-scope)
   :filter (org-glance-view-filter view)))

(cl-defmethod org-glance-view:headlines/formatted ((view org-glance-view))
  "List headlines as formatted strings for VIEW."
  (->> view
    org-glance-view:headlines
    (mapcar #'org-glance-headline:format)
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
    (org-glance-headline:expand-parents)
    (let* ((source --org-glance-view-src)
           (beg --org-glance-view-beg)
           (end --org-glance-view-end)
           (promote-level --org-glance-view-indent)
           (glance-hash --org-glance-view-hash)
           (mat-hash (org-glance-view-subtree-hash))
           (src-hash (org-glance-view-source-hash)))

      (unless (string= glance-hash src-hash)
        (message "Source: %s" source)
        (message "Source hash: %s" src-hash)
        (message "Glance hash: %s" glance-hash)
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
                     (org-glance-headline:demote promote-level)
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

          (with-demoted-errors (run-hooks 'org-glance-after-materialize-sync-hook))
          (message "Materialized view synchronized successfully"))))))

(defun org-glance-view-subtree-hash ()
  (save-restriction
    (org-narrow-to-subtree)
    (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
      (with-temp-buffer
        (org-mode)
        (insert buffer-contents)
        (goto-char (point-min))
        (org-glance-headline:promote)
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
          (org-glance-headline:expand-parents)
          (org-glance-headline:promote)
          (buffer-hash))))))

(cl-defmethod org-glance-view-delete ((view-id symbol))
  (remhash view-id org-glance-views))

(cl-defun org-glance-view:completing-read (&optional (prompt "Choose view: "))
  "Run completing read PROMPT on registered views filtered by TYPE."
  (let ((views (org-glance-view:list-view-ids)))
    (if (> (length views) 1)
        (let ((view (org-completing-read prompt views)))
          (intern view))
      (car views))))

(cl-defun org-glance-read-view (&optional (prompt "Choose view: "))
  "Run completing read PROMPT on registered views filtered by TYPE."
  (gethash (org-glance-view:completing-read prompt) org-glance-views))

(cl-defun org-glance-view-agenda (&optional (view-id (org-glance-view:completing-read)))
  (interactive)
  (let ((org-glance-view-agenda-files
         (cond ;; ((string= view-id org-glance-view-selector:all)
           ;;  (cl-loop for view in (org-glance-view:list-view-ids)
           ;;     collect (org-glance-view:summary-location view)))
           (t (list (org-glance-view:summary-location view-id))))))

    (let ((org-agenda-files org-glance-view-agenda-files))
      (org-agenda-list))

    ;; (with-current-buffer org-agenda-buffer
    ;;   (make-local-variable 'org-agenda-files)
    ;;   (setq-local org-agenda-files org-glance-view-agenda-files))
    ))

(cl-defun org-glance-view-visit
    (&optional
       (view-id (org-glance-view:completing-read)))
  (interactive)
  (find-file (org-glance-view:summary-location view-id)))

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

(defun org-glance-view:get-view-by-id (id)
  (gethash id org-glance-views))

(org-glance-module-provide)
