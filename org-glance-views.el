;; -*- lexical-binding: t -*-

(eval-when-compile
  (require 'cl))

(eval-and-compile
  (require 'aes)
  (require 'load-relative)
  (require 'transient)

  (require 'org)
  (require 'org-element)

  (require 'org-glance-db)
  (require 'org-glance-scope))

;; buffer-locals for materialized views

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

(defcustom org-glance-default-scope '(agenda-with-archives)
  "Default scope for glancing views."
  :group 'org-glance
  :type 'list)

;; locals in materialized buffers

(defvar -org-glance-pwd nil)
(defvar -org-glance-src nil)
(defvar -org-glance-beg nil)
(defvar -org-glance-end nil)
(defvar -org-glance-hash nil)
(defvar -org-glance-indent nil)

(eval-and-compile
  (defvar org-glance-view-mode-map (make-sparse-keymap)
    "Extend `org-mode' map with sync abilities.")
  (define-key org-glance-view-mode-map (kbd "C-x C-s") #'org-glance-view-sync-subtree)
  (define-key org-glance-view-mode-map (kbd "C-c C-v") #'org-glance-view-visit-original-heading)
  (define-key org-glance-view-mode-map (kbd "C-c C-q") #'kill-current-buffer))

(define-error 'org-glance-view-not-modified "No changes made in materialized view" 'user-error)
(cl-defun org-glance-view-not-modified (format &rest args) (signal 'org-glance-view-not-modified (list (apply #'format-message format args))))

(define-error 'org-glance-source-file-corrupted "Source file corrupted, please reread" 'user-error)
(cl-defun org-glance-source-file-corrupted (format &rest args) (signal 'org-glance-source-file-corrupted (list (apply #'format-message format args))))

(define-error 'org-glance-properties-corrupted "Materialized view properties corrupted, please reread" 'user-error)
(cl-defun org-glance-properties-corrupted (format &rest args) (signal 'org-glance-properties-corrupted (list (apply #'format-message format args))))

(define-minor-mode org-glance-view-mode
  "A minor mode to be activated only in materialized view editor."
  nil nil org-glance-view-mode-map)

(defvar org-glance-view-default-type '(all)
  "Default type for all views.")

(cl-defstruct org-glance-view
  id
  (type org-glance-view-default-type)
  (scope org-glance-default-scope))

(defvar org-glance-views (make-hash-table :test 'equal))
(defvar org-glance-view-actions (make-hash-table :test 'equal))
(defvar org-glance-db-directory (concat user-emacs-directory "org-glance"))
(defvar org-glance-materialized-view-buffer "*org-glance materialized view*")

(cl-defmethod org-glance-view ((view-id symbol)) (gethash view-id org-glance-views))
(cl-defmethod org-glance-view ((view-id string)) (org-glance-view (intern view-id)))

(cl-defmethod org-glance-view-db ((view org-glance-view))
  (->> view
       (org-glance-view-id)
       (format "org-glance-%s.el")
       (downcase)
       (format "%s/%s" org-glance-db-directory)))

(cl-defmethod org-glance-view-filter ((view org-glance-view))
  (-partial
   #'(lambda (view headline)
       (-contains?
        (mapcar #'downcase (org-element-property :tags headline))
        (downcase (symbol-name (org-glance-view-id view)))))
   view))

(cl-defmethod org-glance-reread-view (&optional (view-id (org-glance-read-view)))
  (interactive)
  (message "Reread view %s" view-id)
  (let* ((view (gethash view-id org-glance-views))
         (db (org-glance-view-db view))
         (filter (org-glance-view-filter view))
         (scope (org-glance-view-scope view)))
    (org-glance-db-init db (org-glance-scope-headlines scope filter))
    view))

(cl-defmethod org-glance-view-headlines ((view org-glance-view))
  "List headlines as org-elements for VIEW."
  (org-glance-headlines
   :db (org-glance-view-db view)
   :scope (org-glance-view-scope view)
   :filter (org-glance-view-filter view)))

(cl-defmethod org-glance-view-headlines/formatted ((view org-glance-view))
  "List headlines as formatted strings for VIEW."
  (->> view
       org-glance-view-headlines
       (mapcar #'org-glance-format)
       (mapcar #'(lambda (hl) (format "[%s] %s" (org-glance-view-id view) hl)))))

(cl-defmethod org-glance-view-prompt ((view org-glance-view) (action symbol))
  (s-titleize (format "%s %s: " action (org-glance-view-id view))))

(cl-defmethod org-glance-view-action-resolve ((view org-glance-view) (action symbol))
  (let* ((action-types (->> org-glance-view-actions
                            (gethash action)
                            (-sort (lambda (lhs rhs) (> (length lhs) (length rhs))))))
         (view-actions (loop for action-type in action-types
                             with view-type = (org-glance-view-type view)
                             when (cl-subsetp action-type view-type)
                             return action-type)))
    (or view-actions
        (car (member org-glance-view-default-type (gethash action org-glance-view-actions))))))

(defun org-glance-act-arguments nil
  (transient-args 'org-glance-act))

(defun org-glance-list-views ()
  "List registered views."
  (hash-table-keys org-glance-views))

(cl-defmacro org-glance-with-headline-narrowed (headline &rest forms)
  (declare (indent defun))
  `(let* ((file (org-element-property :file ,headline))
          (file-buffer (get-file-buffer file))
          (visited-buffer (current-buffer)))
     (org-glance-call-action 'visit :on ,headline)
     (widen)
     (org-narrow-to-subtree)
     (unwind-protect
         (let ((org-link-frame-setup (cl-acons 'file 'find-file org-link-frame-setup)))
           ,@forms)
       (widen))
     (cond ((and file-buffer (not (eq file-buffer (current-buffer)))) (bury-buffer file-buffer))
           ((and file-buffer (eq file-buffer (current-buffer))) (progn (switch-to-buffer visited-buffer)
                                                                       (bury-buffer file-buffer)))
           (t (kill-buffer (get-file-buffer file))))))

(cl-defmacro org-glance-with-headline-materialized (headline &rest forms)
  (declare (indent defun))
  `(let* ((file (org-element-property :file ,headline))
          (file-buffer (get-file-buffer file)))
     (org-glance-call-action 'materialize :on ,headline)
     (unwind-protect
         (let ((org-link-frame-setup (cl-acons 'file 'find-file org-link-frame-setup)))
           ,@forms)
       (kill-buffer org-glance-materialized-view-buffer))
     (cond (file-buffer (bury-buffer file-buffer))
           (t (kill-buffer (get-file-buffer file))))))

(cl-defmethod org-glance-export-view
  (&optional (view-id (org-glance-read-view))
             (destination (read-directory-name "Export destination: "))
             force)
  (interactive)
  (let* ((headlines (->> view-id
                         org-glance-reread-view
                         org-glance-view-headlines))
         (filename (format "%s.org" view-id))
         (dest-file-name (f-join destination filename)))
    (when (and
           (file-exists-p dest-file-name)
           (or force (y-or-n-p (format "File %s already exists. Overwrite?" dest-file-name))))
      (delete-file dest-file-name t))
    (loop for headline in headlines
          do (org-glance-with-headline-materialized headline
               (append-to-file (point-min) (point-max) dest-file-name)
               (append-to-file "\n" nil dest-file-name)))
    (if force
        dest-file-name
      (find-file dest-file-name))))

(cl-defmethod org-glance-export-all-views
  (&optional (destination (read-directory-name "Export destination: ")))
  (interactive)
  (loop for view-id being the hash-keys of org-glance-views
        do (org-glance-export-view view-id destination 'force)))

;; some private helpers

(defun org-glance-encrypt-subtree (&optional password)
  "Encrypt subtree at point with PASSWORD."
  (interactive)
  (let* ((beg (save-excursion (org-end-of-meta-data) (point)))
         (end (save-excursion (org-end-of-subtree t)))
         (plain (let ((plain (buffer-substring-no-properties beg end)))
                  (if (with-temp-buffer
                        (insert plain)
                        (aes-is-encrypted))
                      (user-error "Headline is already encrypted")
                    plain)))
         (encrypted (aes-encrypt-buffer-or-string plain password)))
    (save-excursion
      (org-end-of-meta-data)
      (kill-region beg end)
      (insert encrypted))))

(defun org-glance-decrypt-subtree (&optional password)
  "Decrypt subtree at point with PASSWORD."
  (interactive)
  (let* ((beg (save-excursion (org-end-of-meta-data) (point)))
         (end (save-excursion (org-end-of-subtree t)))
         (encrypted (let ((encrypted (buffer-substring-no-properties beg end)))
                      (if (not (with-temp-buffer
                                 (insert encrypted)
                                 (aes-is-encrypted)))
                          (user-error "Headline is not encrypted")
                        encrypted)))
         (plain (aes-decrypt-buffer-or-string encrypted password)))
    (unless plain
      (user-error "Wrong password"))
    (save-excursion
      (org-end-of-meta-data)
      (kill-region beg end)
      (insert plain))))

(defun -org-glance-promote-subtree ()
  (let ((promote-level 0))
    (cl-loop while (condition-case nil
                       (org-with-limited-levels (org-map-tree 'org-promote) t)
                     (error nil))
             do (incf promote-level))
    promote-level))

(defun -org-glance-demote-subtree (level)
  (cl-loop repeat level
           do (org-with-limited-levels
               (org-map-tree 'org-demote))))

(defun -org-glance-first-level-heading ()
  (save-excursion
    (unless (org-at-heading-p) (org-back-to-heading))
    (beginning-of-line)
    (point)))

(defun -org-glance-end-of-meta-data ()
  (save-excursion
    (org-end-of-meta-data)
    (point)))

(defun -element-at-point-equals-headline (headline)
  (condition-case nil
      (s-contains? (org-element-property :raw-value (org-element-at-point))
                   (org-element-property :raw-value headline))
    (error nil)))

(cl-defmethod org-glance-read-view (&optional (prompt "Choose view: "))
  "Run completing read PROMPT on registered views filtered by TYPE."
  (let ((views (org-glance-list-views)))
    (if (> (length views) 1)
        (intern (org-completing-read prompt views))
      (car views))))

(cl-defun org-glance-call-action (name &key (on 'current-headline) (for 'all))
  (when (eq on 'current-headline)
    (setq on (org-element-at-point)))
  (let ((fn (intern (format "org-glance--%s--%s" name for))))
    (unless (fboundp fn)
      (user-error "Unbound function %s" fn))
    (funcall fn on)))

(cl-defmethod org-glance-register-action ((name symbol) (type symbol))
  (org-glance-register-action name (list type)))

(cl-defmethod org-glance-register-action ((name symbol) (type list))
  (let ((type (cl-pushnew type (gethash name org-glance-view-actions) :test #'seq-set-equal-p)))
    (puthash name type org-glance-view-actions)))

(cl-defmethod org-glance-generic-method-name ((name symbol))
  (intern (format "org-glance-action-%s" name)))

(cl-defmethod org-glance-concrete-method-name ((name symbol) (type symbol))
  (org-glance-concrete-method-name name (list type)))

(cl-defmethod org-glance-concrete-method-name ((name symbol) (type list))
  (->> type
       (-map #'symbol-name)
       (-sort #'s-less?)
       (s-join "-")
       (format "org-glance-action-%s-%s" name)
       (intern)))

(cl-defun org-glance-headlines-for-action (action)
  (loop for view being the hash-values of org-glance-views
        when (org-glance-view-action-resolve view action)
        append (mapcar #'(lambda (headline) (cons headline view)) (org-glance-view-headlines/formatted view))))

(cl-defmacro org-glance-def-action (name args _ type &rest body)
  "Defun method NAME (ARGS) BODY.
Make it accessible for views of TYPE in `org-glance-view-actions'."
  (declare (debug
            ;; Same as defun but use cl-lambda-list.
            (&define [&or name ("setf" :name setf name)]
                     cl-lambda-list
                     symbolp
                     cl-declarations-or-string
                     [&optional ("interactive" interactive)]
                     def-body))
           (doc-string 6)
           (indent 4))
  (org-glance-register-action name type)
  (let* ((res (cl--transform-lambda (cons args body) name))
         (generic-func-name (org-glance-generic-method-name name))
         (concrete-func-name (org-glance-concrete-method-name name type))
         (action-private-method (intern (format "org-glance--%s--%s" name type)))
	 (form `(progn
                  (unless (fboundp (quote ,generic-func-name))
                    (defun ,generic-func-name (&optional args)
                      (interactive (list (org-glance-act-arguments)))
                      (let* ((action (quote ,name))
                             (headlines (org-glance-headlines-for-action action))
                             (choice (org-completing-read (format "%s: " action) headlines))
                             (view (alist-get choice headlines nil nil #'string=))
                             (method-name (->> action
                                               (org-glance-view-action-resolve view)
                                               (org-glance-concrete-method-name action)))
                             (headline (s-replace-regexp "^\\[.*\\] " "" choice)))
                        (funcall method-name args view headline))))

                  (defun ,concrete-func-name (&optional args view headline)
                    (interactive (list (org-glance-act-arguments)))
                    args
                    (org-glance
                     :default-choice headline
                     :scope (org-glance-view-scope view)
                     :prompt (org-glance-view-prompt view (quote ,name))
                     :db (org-glance-view-db view)
                     :filter (org-glance-view-filter view)
                     :action (function ,action-private-method)))

                  (defun ,action-private-method
                      ,@(cdr res)))))

    (if (car res) `(progn ,(car res) ,form) form)))

;; (org-glance-def-type all "Doc string")
;; (org-glance-def-type crypt)
;; (org-glance-def-type kvs)

;; (org-glance-def-action ... for type)

;; (org-glance-def-capture (headline) for type

(org-glance-def-action visit (headline) :for all
  "Visit HEADLINE."
  (let* ((file (org-element-property :file headline))
         (point (org-element-property :begin headline))
         (buffer (get-file-buffer file)))
    (cond ((file-exists-p file) (find-file file))
          (t (org-glance-db-outdated "File not found: %s" file)))
    (widen)
    (goto-char point)
    (cond ((-element-at-point-equals-headline headline)
           (cl-loop while (org-up-heading-safe)) ;; expand parents
           (org-narrow-to-subtree)
           (outline-show-branches)
           (widen)
           (goto-char point)
           (outline-show-subtree))
          (t (unless buffer
               (kill-buffer))
             (org-glance-db-outdated "Cache file is outdated")))))

(org-glance-def-action materialize (headline) :for all
  "Materialize HEADLINE in separate buffer."
  (cl-labels ((first-level-heading () (save-excursion
                                        (unless (org-at-heading-p) (org-back-to-heading))
                                        (beginning-of-line)
                                        (point)))
              (end-of-subtree () (save-excursion (org-end-of-subtree t)))
              (buffer-contents (beg end) (->> (buffer-substring-no-properties beg end)
                                              (s-trim))))
    (let ((buffer org-glance-materialized-view-buffer))
      (save-window-excursion
        (org-glance-call-action 'visit :on headline)
        (let* ((file (org-element-property :file headline))
               (beg (first-level-heading))
               (end (end-of-subtree))
               (contents (buffer-contents beg end)))
          (when (get-buffer buffer)
            (switch-to-buffer buffer)
            (condition-case nil
                (org-glance-view-sync-subtree)
              (org-glance-view-not-modified nil))
            (kill-buffer buffer))
          (with-current-buffer (get-buffer-create buffer)
            (delete-region (point-min) (point-max))
            (org-mode)
            (org-glance-view-mode)
            (insert contents)
            (goto-char (point-min))
            (org-content 1)
            (org-cycle-hide-drawers 'all)
            (setq-local -org-glance-src file)
            (setq-local -org-glance-beg beg)
            (setq-local -org-glance-end end)
            ;; extract hash from promoted subtree
            (setq-local -org-glance-hash (org-glance-view-subtree-hash))
            ;; run hooks on original subtree
            (with-demoted-errors (run-hooks 'org-glance-after-materialize-hook))
            ;; then promote it saving original level
            (setq-local -org-glance-indent (-org-glance-promote-subtree)))
          (org-cycle 'contents)))
      (switch-to-buffer buffer))))

(org-glance-def-action open (headline) :for link
  "Search for `org-any-link-re' under the HEADLINE
then run `org-completing-read' to open it."
  (org-glance-with-headline-narrowed headline
    (let* ((links (org-element-map (org-element-parse-buffer) 'link
                    (lambda (link)
                      (cons
                       (substring-no-properties
                        (or (nth 2 link) ;; link alias
                            (org-element-property :raw-link link))) ;; full link if alias is none
                       (org-element-property :begin link)))))
           (point (cond
                   ((> (length links) 1) (cdr (assoc (org-completing-read "Open link: " links) links)))
                   ((= (length links) 1) (cdar links))
                   (t (user-error "Unable to find links in %s" (buffer-file-name))))))
      (goto-char point)
      (org-open-at-point))))

(org-glance-def-action extract-property (headline) :for kvs
  "Completing read all properties from HEADLINE and its successors to kill ring."
  (save-window-excursion
    (org-glance-call-action 'materialize :on headline)
    (org-glance-buffer-properties-to-kill-ring)))

(org-glance-def-action materialize (headline) :for crypt
  "Decrypt encrypted HEADLINE, then call MATERIALIZE action on it."
  (cl-flet ((decrypt ()
                     (setq-local -org-glance-pwd (read-passwd "Password: "))
                     (org-glance-decrypt-subtree -org-glance-pwd)))
    (add-hook 'org-glance-after-materialize-hook #'decrypt t)
    (unwind-protect
        (progn
          (org-glance-call-action 'materialize :on headline)
          (org-cycle-hide-drawers 'all))
      (remove-hook 'org-glance-after-materialize-hook #'decrypt)))
  (add-hook 'org-glance-before-materialize-sync-hook
            (lambda ()
              (-org-glance-demote-subtree -org-glance-indent)
              (org-glance-encrypt-subtree -org-glance-pwd)
              (-org-glance-promote-subtree))
            'append 'local)
  (add-hook 'org-glance-after-materialize-sync-hook
            (lambda ()
              (-org-glance-demote-subtree -org-glance-indent)
              (org-glance-decrypt-subtree -org-glance-pwd)
              (-org-glance-promote-subtree))
            'append 'local))

(org-glance-def-action extract-property (headline) :for (kvs crypt)
  "Materialize HEADLINE, decrypt it, then run completing read on all properties to kill ring."
  (save-window-excursion
    (org-glance-call-action 'materialize :on headline :for 'crypt)
    (org-cycle-hide-drawers 'all)
    (unwind-protect
        (org-glance-buffer-properties-to-kill-ring)
      (kill-buffer org-glance-materialized-view-buffer))))

(defun org-glance-buffer-properties-to-kill-ring ()
  (while t
    (let* ((properties (org-buffer-property-keys))
           (property (org-completing-read "Extract property: " properties))
           (values (org-property-values property)))
      (kill-new (cond
                 ((> (length values) 1) (org-completing-read "Choose property value: " values))
                 ((= (length values) 1) (car values))
                 (t (user-error "Something went wrong: %s" values)))))))

(defun org-glance-view-visit-original-heading ()
  (interactive)
  (save-excursion
    (cl-loop while (org-up-heading-safe))
    (let* ((heading (list :file -org-glance-src
                          :begin -org-glance-beg
                          :raw-value (org-element-property :raw-value (org-element-at-point))))
           (virtual-element (org-element-create 'headline heading)))
      (org-glance-call-action 'visit :on virtual-element))))

(defun org-glance-view-sync-subtree ()
  (interactive)
  (save-excursion
    (cl-loop while (org-up-heading-safe))
    (let* ((source -org-glance-src)
           (beg -org-glance-beg)
           (end -org-glance-end)
           (promote-level -org-glance-indent)
           (glance-hash -org-glance-hash)
           (mat-hash (org-glance-view-subtree-hash))
           (src-hash (org-glance-view-source-hash)))

      (unless (string= glance-hash src-hash)
        (org-glance-source-file-corrupted source))

      (when (string= glance-hash mat-hash)
        (org-glance-view-not-modified source))

      (when (y-or-n-p "Subtree has been modified. Apply changes?")
        (with-demoted-errors (run-hooks 'org-glance-before-materialize-sync-hook))

        (let ((new-contents
               (save-restriction
                 (org-narrow-to-subtree)
                 (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
                   (with-temp-buffer
                     (org-mode)
                     (insert buffer-contents)
                     (goto-char (point-min))
                     (-org-glance-demote-subtree promote-level)
                     (buffer-substring-no-properties (point-min) (point-max)))))))

          (with-temp-file source
            (org-mode)
            (insert-file-contents source)
            (delete-region beg end)
            (goto-char beg)
            (insert new-contents)
            (setq end (point)))

          (setq-local -org-glance-beg beg)
          (setq-local -org-glance-end end)
          (setq-local -org-glance-hash (org-glance-view-source-hash))

          (with-demoted-errors (run-hooks 'org-glance-after-materialize-sync-hook)))))))

(defun org-glance-view-subtree-hash ()
  (save-restriction
    (org-narrow-to-subtree)
    (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
      (with-temp-buffer
        (org-mode)
        (insert buffer-contents)
        (goto-char (point-min))
        (-org-glance-promote-subtree)
        (buffer-hash)))))

(defun org-glance-view-source-hash ()
  (let ((src -org-glance-src)
        (beg -org-glance-beg)
        (end -org-glance-end))
    (with-temp-buffer
      (insert-file-contents src)
      (let ((subtree (condition-case nil
                         (buffer-substring-no-properties beg end)
                       (error (org-glance-properties-corrupted "Materialized properties corrupted, please reread")))))
        (with-temp-buffer
          (org-mode)
          (insert (s-trim subtree))
          (cl-loop while (org-up-heading-safe))
          (-org-glance-promote-subtree)
          (buffer-hash))))))

(cl-defmacro org-glance-def-view (view-id &key type scope &allow-other-keys)
  `(progn
     (unless (eq nil (gethash (quote ,view-id) org-glance-views))
       (user-error "View %s is already registered." (quote ,view-id)))
     (let ((view (make-org-glance-view :id (quote ,view-id))))
       (when ,scope (setf (org-glance-view-scope view) ,scope))
       (when ,type  (setf (org-glance-view-type view) ,type))
       (puthash (quote ,view-id) view org-glance-views)
       (message "%s view is now ready to glance" (quote ,view-id))
       view)))

(cl-defmethod org-glance-remove-view ((view-id symbol))
  (remhash view-id org-glance-views))

(cl-defmacro org-glance-let (view-id &rest forms &key type scope (as 'view) &allow-other-keys)
  (declare (indent defun))
  `(let (result
         (,as (org-glance-def-view ,view-id :type ,type :scope ,scope)))
     (unwind-protect
         (setq result (progn ,@forms))
       (org-glance-remove-view (quote ,view-id)))
     result))

(defun org-glance-capture-subtree-at-point ()
  (interactive)
  (unless (org-at-heading-p) (org-back-to-heading))
  ;; (let* ((other-views (seq-difference
  ;;                      (org-glance-list-views)
  ;;                      (mapcar #'intern (org-get-tags))))
  ;;        (view-id (org-completing-read "View: " other-views))
  ;;        (view (org-glance-view view-id)))
  ;;   (org-toggle-tag view-id)
  ;;   ;; (loop for type in (org-glance-view-type view)
  ;;   ;;       do (pp type))
  ;;   )
  )

(provide-me)
;;; org-glance-views.el ends here
