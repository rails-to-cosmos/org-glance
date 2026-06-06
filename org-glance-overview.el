;; -*- lexical-binding: t -*-

(require 'org-attach)
(require 'org-capture)
(require 'ol)
(require 'org-agenda)

(require 'org-glance-headline)
(require 'org-glance-metadata)
(require 'org-glance-tag)
(require 'org-glance-utils)
;; Provides `org-glance:with-headline-materialized' (a macro -- needed at
;; compile time) and `org-glance-headline:materialize'.
(require 'org-glance-material-mode)

;; Defined in org-glance.el, which requires this file (cycle) -- runtime-only refs.
(declare-function org-glance-capture "org-glance" (tag &rest args))
(declare-function org-glance-tags:completing-read "org-glance" (&optional prompt require-match))
(declare-function org-glance:create-tag "org-glance" (tag))
(declare-function org-glance:make-tag-directory "org-glance" (&optional tag))
(declare-function org-glance:tag-file-name "org-glance" (&optional tag))
(declare-function org-glance:tags-sorted "org-glance")
(declare-function org-glance:open "org-glance" (&optional headline))
(declare-function org-glance-headline:make-directory "org-glance" (location title))

;; Defined in org-glance.el (cycle) -- the registry of known tags.
(defvar org-glance-tags)

(defcustom org-glance-clocktable-properties
  (list :maxlevel 2
        :properties '("CLOSED" "SCHEDULED")
        :link t)
  "Default clocktable properties for glance overview."
  :group 'org-glance
  :type 'plist)

(defconst org-glance-overview:header
  "#    -*- mode: org; mode: org-glance-overview -*-

${category}
${todo-states}
${todo-order}

")

(defvar org-glance-overview-mode-map (make-sparse-keymap)
  "Manipulate `org-mode' entries in `org-glance-overview-mode'.")

;;; medium methods applied for all first-level headlines in current file

(cl-defun org-glance-overview:choose-headline ()
  "Choose `org-glance-headline' from current overview buffer and move cursor to it."
  (let* ((headlines (org-glance-headline:buffer-headlines (current-buffer)))
         (headlines-active (--filter (org-glance-headline:active? it) headlines))
         (titles (mapcar #'org-glance-headline:plain-title headlines-active))
         (title (completing-read "Specify headline: " titles nil t))
         (headline (org-glance-headline:select-by-title title headlines))
         (id (org-glance-headline:id headline)))
    (org-glance-headline:search-buffer-by-id id)))

(cl-defmacro org-glance-overview:apply-on-headline (&rest forms)
  "Eval FORMS on headline at point.
If point is not at a headline, prompt the user to choose one first."
  (declare (indent 0) (debug t))
  `(org-glance:interactive-lambda
     (when (org-before-first-heading-p)
       (org-glance-overview:choose-headline))
     ,@forms))

(define-key org-glance-overview-mode-map (kbd ";") #'org-glance-overview:archive)
(define-key org-glance-overview-mode-map (kbd ",") #'beginning-of-buffer)
(define-key org-glance-overview-mode-map (kbd "<") #'beginning-of-buffer)
(define-key org-glance-overview-mode-map (kbd ".") #'end-of-buffer)
(define-key org-glance-overview-mode-map (kbd ">") #'end-of-buffer)
(define-key org-glance-overview-mode-map (kbd "^") #'org-glance-overview:order)

(define-key org-glance-overview-mode-map (kbd "RET")
            (org-glance-overview:apply-on-headline
              (org-glance-overview:materialize-headline)))

(define-key org-glance-overview-mode-map (kbd "#")
            (org-glance-overview:apply-on-headline
              (let ((headline (org-glance-overview:original-headline)))
                (if (org-glance-headline:encrypted? headline)
                    (progn
                      (org-glance-headline:with-narrowed-headline headline
                        (org-glance-headline:decrypt)
                        (save-buffer))
                      (org-glance-overview:pull))
                  (org-glance:with-headline-materialized headline
                    (org-glance-headline:encrypt))))))

(define-key org-glance-overview-mode-map (kbd "/")
            (org-glance:interactive-lambda
              (org-glance-overview:choose-headline)))

(define-key org-glance-overview-mode-map (kbd "F")
            (org-glance-overview:apply-on-headline
              (org-attach-reveal-in-emacs)))

(define-key org-glance-overview-mode-map (kbd "g")
            (org-glance:interactive-lambda
              (if (org-before-first-heading-p)
                  (progn
                    (org-glance-overview:refresh-widgets (org-glance-overview:tag))
                    (org-glance-overview:order)
                    (pulse-momentary-highlight-region
                     (point-min)
                     (save-excursion
                       (org-next-visible-heading 1)
                       (point))
                     'region))
                (org-glance-overview:pull)
                (outline-show-subtree)
                (org-cycle-hide-drawers 'org-cycle-hide-drawers))
              (save-buffer)))

(define-key org-glance-overview-mode-map (kbd "j")
            (org-glance-overview:apply-on-headline
              (org-glance-overview:jump-headline)))

(define-key org-glance-overview-mode-map (kbd "a") #'org-glance-overview:agenda)
(define-key org-glance-overview-mode-map (kbd "n") #'outline-next-heading)
(define-key org-glance-overview-mode-map (kbd "p") #'outline-previous-heading)
(define-key org-glance-overview-mode-map (kbd "q") #'bury-buffer)

(define-key org-glance-overview-mode-map (kbd "k")
            (org-glance-overview:apply-on-headline
              (org-glance-overview:kill-headline)))

(define-key org-glance-overview-mode-map (kbd "R")
            (org-glance-overview:apply-on-headline
              (org-glance-overview:move)))

;; (define-key org-glance-overview-mode-map (kbd "z") #'org-glance-overview:vizualize)

(define-key org-glance-overview-mode-map (kbd "+")
            (org-glance:interactive-lambda
              (org-glance-capture (org-glance-overview:tag))))

;; (define-key org-glance-overview-mode-map (kbd "*") #'org-glance-overview:import-headlines-from-directory)

(defcustom org-glance-overview:default-state-ordering
  (list
   "started"
   "pending"
   "todo"
   ""
   "done"
   "cancelled")
  "State-related ordering."
  :group 'org-glance
  :type '(repeat string))

(cl-defun org-glance-overview:state-ordering (tag)
  (cl-check-type tag org-glance-tag)

  (let ((config (f-join (org-glance-overview:directory tag) "task-states.el")))
    (if (and (file-exists-p config) (file-readable-p config))
        (with-temp-buffer
          (insert-file-contents config)
          (read (buffer-substring-no-properties (point-min) (point-max))))
      org-glance-overview:default-state-ordering)))

(cl-defun org-glance-overview:partition-mapper (tag)
  "Main method for partitioning headlines in overview."
  (cl-check-type tag org-glance-tag)

  (let ((ordering (org-glance-overview:state-ordering tag)))
    (list (not (org-in-archived-heading-p)) ;; partition by ARCHIVED. "not" means archived headlines should be in a bottom
          (not (org-in-commented-heading-p)) ;; partition by COMMENTED. "not" means commented headlines should be in a bottom
          (or (-elem-index (downcase (or (org-element-property :todo-keyword (org-element-at-point)) "")) ordering) 0) ;; partition by state
          ;; (downcase (s-join ":" (sort (org-get-tags) #'string<))) ;; partition by tag string.
          (or (org-element-property :priority (org-element-at-point)) ?B))))

(cl-defun org-glance-overview:partition-comparator (lhs rhs)
  "Main method to compare LHS with RHS."
  (cl-check-type lhs org-glance-headline)
  (cl-check-type rhs org-glance-headline)

  (cl-loop
   for (i j) in (-zip-lists lhs rhs)
   when (cond ((not (eql (type-of i) (type-of j))) nil)
              ((stringp i) (not (string= i j)))
              (t (not (eql i j))))
   return (cond ((stringp i) (string< i j))
                ((numberp i) (< i j))
                ((booleanp i) i)
                (t nil))))

(cl-defun org-glance-overview:partition (&key using (test #'equal) (comparator #'<))
  (declare (indent 0))
  (let ((buffers (make-hash-table :test test)))
    (save-excursion
      (goto-char (point-min))
      (outline-next-heading)
      (while (< (point) (point-max))
        (let* ((group-state (funcall using))
               (group-buffer (get-buffer-create (concat "org-glance-overview-group:" (prin1-to-string group-state))))
               (contents (s-trim (buffer-substring-no-properties (point) (save-excursion (org-end-of-subtree t t))))))
          (with-current-buffer group-buffer
            (org-mode)
            (unless (gethash group-state buffers)
              (delete-region (point-min) (point-max)))
            (insert contents "\n"))
          (puthash group-state group-buffer buffers)
          (outline-next-heading))))
    (cl-loop for key in (sort (hash-table-keys buffers) comparator)
             collect (gethash key buffers))))

(cl-defun org-glance-overview:register-headline-in-metadata (headline tag)
  (cl-check-type headline org-glance-headline)
  (cl-check-type tag org-glance-tag)

  ;; TODO implement explicit metadata model
  (let ((metadata-file-name (org-glance-metadata:location tag))
        (metadata (org-glance-metadata:read-tag-metadata tag)))
    (org-glance-metadata:add-headline headline metadata)
    (org-glance-metadata:save metadata metadata-file-name)))

(cl-defun org-glance-overview:remove-headline-from-metadata (headline tag)
  (cl-check-type headline org-glance-headline)
  (cl-check-type tag org-glance-tag)

  ;; TODO implement explicit model for metadata
  (let ((metadata-location (org-glance-metadata:location tag))
        (metadata (org-glance-metadata:read-tag-metadata tag)))
    (org-glance-metadata:remove-headline headline metadata)
    (org-glance-metadata:save metadata metadata-location)))

(cl-defun org-glance-overview:register-headline-in-overview (headline tag)
  "Add HEADLINE overview to TAG file."
  (cl-check-type headline org-glance-headline)
  (cl-check-type tag org-glance-tag)

  (org-glance--with-file-visited (org-glance-overview:file-name tag)
    (save-restriction
      (widen)

      (condition-case nil
          (org-glance-overview:remove-headline-from-overview headline tag)
        (org-glance-headline:not-found! nil))

      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert "\n" (org-glance-headline:overview headline) "\n")
        (save-buffer))))
  headline)

(cl-defun org-glance-overview:remove-headline-from-overview (headline tag)
  "Add HEADLINE clone in overview VIEW-ID file."
  (cl-check-type headline org-glance-headline)
  (cl-check-type tag org-glance-tag)

  (save-window-excursion
    (org-glance-overview tag)
    (save-restriction
      (widen)
      (save-excursion
        (when (condition-case nil
                  (org-glance-headline:search-buffer-by-id (org-glance-headline:id headline))
                (org-glance-headline:not-found! nil))
          (let ((inhibit-read-only t))
            (delete-region (org-entry-beginning-position) (save-excursion (org-end-of-subtree t t)))
            (save-buffer)))))))

(cl-defun org-glance-overview:register-headline-in-archive (headline tag)
  "Add HEADLINE overview to CLASS archive."
  (cl-check-type headline org-glance-headline)
  (cl-check-type tag org-glance-tag)
  (org-glance-overview:ensure-archive tag)
  (org-glance--with-file-visited (org-glance-overview:archive-location tag)
    (save-restriction
      (widen)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert "\n" (org-glance-headline:overview headline) "\n")
        (save-buffer))))
  headline)

(cl-defun org-glance-capture-headline-at-point (&optional (tag (org-glance-tags:completing-read "Tag: ")))
  "Extract all possible information from headline at point."
  (declare (indent 1))
  (interactive)
  (save-window-excursion
    (save-excursion
      (org-glance--back-to-heading)
      (let* ((id (org-glance-tag:generate-id tag))
             (dir (org-glance:make-tag-directory tag))
             (output-file (f-join dir (format "%s.org" (org-glance-tag:to-string tag)))))

        (mkdir dir 'parents)

        (save-restriction
          (org-narrow-to-subtree)
          (let* ((contents (buffer-substring-no-properties (point-min) (point-max)))
                 (result (save-window-excursion
                           (find-file output-file)
                           (save-restriction
                             (widen)
                             (goto-char (point-max))
                             (save-excursion
                               (insert contents))
                             (org-glance-headline:promote-to-the-first-level)
                             (org-set-property "ORG_GLANCE_ID" id)
                             (org-set-property "DIR" dir)
                             (org-set-property "CATEGORY" (org-glance-tag:to-string tag))
                             (org-set-property "ORG_GLANCE_CREATION_TIME" (with-temp-buffer
                                                                            (let ((current-prefix-arg '(16)))
                                                                              (call-interactively #'org-time-stamp-inactive)
                                                                              (buffer-substring-no-properties (point-min) (point-max)))))
                             (unless (member tag (org-glance-tag:from-headline-at-point))
                               (org-toggle-tag (org-glance-tag:to-string tag)))
                             (save-buffer)
                             (org-glance-headline:at-point)))))
            result))))))

(cl-defun org-glance-capture:prepare-finalize-hook (id tag)
  "Preprocess headline before capturing"
  (goto-char (point-min))
  (or (org-at-heading-p) (org-next-visible-heading 0))
  (org-set-property "ORG_GLANCE_ID" id)
  (org-glance:create-tag tag)
  (org-toggle-tag (format "%s" tag) t))

(cl-defun org-glance-capture:after-finalize-hook (id tag)
  "Register captured headline ID in the system tagged by TAG."
  (when-let (headline (org-glance-headline:search-buffer-by-id id))
    (let* ((title (org-glance-headline:plain-title headline))
           (tag-file (org-glance:tag-file-name tag))
           (refile-dir (org-glance-headline:make-directory tag-file title))
           (tmp-file (org-glance-headline:file-name headline))
           (new-file (org-glance--make-file-directory (f-join refile-dir (format "%s.org" tag)))))
      ;; (org-set-property "DIR" (abbreviate-file-name refile-dir))
      ;; (save-buffer)
      ;; (kill-buffer)

      (f-move tmp-file new-file)
      (org-glance-headline:update headline
        :file new-file
        :dir (abbreviate-file-name refile-dir))
      (org-glance-overview tag)
      (org-glance-overview:register-headline-in-metadata headline tag)
      (org-glance-overview:register-headline-in-overview headline tag)
      ;; (org-overview)
      ;; (org-glance-headline:search-buffer-by-id id)
      )))

(define-minor-mode org-glance-overview-mode
  "A minor read-only mode to use in overview files."
  :global nil
  :init-value nil
  :keymap org-glance-overview-mode-map
  :after-hook (progn
                (org-overview)
                (read-only-mode +1)))

(defvar org-glance-edit-mode-map (make-sparse-keymap)
  "Edit entries in `org-glance-edit-mode'.")

;; (define-key org-glance-overview-mode-map (kbd "C-c C-p") #'org-glance-edit-mode:start)
;; (define-key org-glance-edit-mode-map (kbd "C-c C-c") 'org-glance-edit-mode:apply)

(define-minor-mode org-glance-edit-mode
  "A minor mode to edit and sync overview files."
  :global nil
  :init-value nil
  :keymap org-glance-edit-mode-map)

;; (cl-defun org-glance-edit-mode:start ()
;;   (interactive)
;;   (org-glance-edit-mode +1)
;;   (org-glance-overview-mode -1)
;;   (defvar-local org-glance-overview:changed-headlines '())
;;   (cl-pushnew 'org-glance-overview:track-changes after-change-functions)
;;   ;; (add-hook 'before-save-hook #'org-glance-overview:sync-headlines t t)
;;   (message "Edit mode is now enabled."))

;; (cl-defun org-glance-edit-mode:apply ()
;;   (interactive)
;;   (org-glance-overview:sync-headlines)
;;   (setq-local after-change-functions (cl-remove 'org-glance-overview:track-changes after-change-functions))
;;   (org-glance-edit-mode -1)
;;   (org-glance-overview-mode +1)
;;   (message "All changes have been applied."))

(cl-defun org-glance-overview:directory (&optional (tag (org-glance-tags:completing-read)))
  "Path to file where CLASS headlines are stored."
  (cl-check-type tag org-glance-tag)
  (abbreviate-file-name (f-join org-glance-directory (org-glance-tag:to-string tag))))

(cl-defun org-glance-overview:file-name (&optional (tag (org-glance-tags:completing-read)))
  "Path to file where VIEW-ID headlines are stored."
  (cl-check-type tag org-glance-tag)
  (format "%s/%s/%s.org" org-glance-directory tag tag))

(cl-defun org-glance-overview:archive-location (&optional (tag (org-glance-tags:completing-read)))
  "Path to file where VIEW-ID headlines are stored."
  (cl-check-type tag org-glance-tag)
  (format "%s/%s/%s.org_archive" org-glance-directory tag tag))

(cl-defun org-glance:capture-template (tag &key (default ""))
  (cl-check-type tag org-glance-tag)
  (let ((capture-template-config-file (f-join (org-glance-overview:directory tag) "template.org")))
    (s-replace "%?" (concat default "%?")
               (cond ((f-exists-p capture-template-config-file) (with-temp-buffer
                                                                  (insert-file-contents capture-template-config-file)
                                                                  (buffer-substring-no-properties (point-min) (point-max))))
                     (t "* %?")))))

(cl-defun org-glance-overview:refresh-widgets (tag)
  (cl-check-type tag org-glance-tag)
  (save-excursion
    (let* ((inhibit-read-only t)
           (inhibit-message t)
           (category (format "#+CATEGORY: %s" tag))
           (todo-states (cl-loop for todo-seq in org-todo-keywords
                                 concat (concat "#+TODO: " (s-join " " (cdr todo-seq)) "\n")
                                 into result
                                 finally return (s-trim result)))

           (todo-order (concat "#+TODO_ORDER: " (cl-loop for state in (org-glance-overview:state-ordering tag)
                                                         if (string-empty-p state)
                                                         concat "_ " into result
                                                         else
                                                         concat (concat (upcase state) " ") into result
                                                         finally return (s-trim result))))
           (header (s-replace-all `(("${category}" . ,category)
                                    ("${todo-states}" . ,todo-states)
                                    ("${todo-order}" . ,todo-order))
                                  org-glance-overview:header)))

      (goto-char (point-min))
      (org-next-visible-heading 1)
      (delete-region (point-min) (point))
      (insert header)
      (goto-char (point-min))
      (org-next-visible-heading 1)
      (backward-char)
      (let ((org-clock-clocktable-default-properties org-glance-clocktable-properties))
        (org-clock-report)))))

(cl-defun org-glance-overview:create-archive (tag)
  (cl-check-type tag org-glance-tag)
  (let ((filename (org-glance--make-file-directory (org-glance-overview:archive-location tag))))
    (with-temp-file filename
      (org-glance-overview:refresh-widgets tag))
    filename))

(cl-defun org-glance-overview:ensure-archive (tag)
  (cl-check-type tag org-glance-tag)
  (let ((filename (org-glance--make-file-directory (org-glance-overview:archive-location tag))))
    (unless (file-exists-p filename)
      (with-temp-file filename
        (org-glance-overview:refresh-widgets tag)))
    filename))

(cl-defun org-glance-overview:create (tag)
  (cl-check-type tag org-glance-tag)
  (let ((overview-file-name (org-glance-overview:file-name tag)))
    (unless (f-exists? overview-file-name)
      (let ((metadata-file-name (org-glance-metadata:location tag))
            (overview-file-name (org-glance-overview:file-name tag)))
        (org-glance--make-file-directory overview-file-name)
        (org-glance-metadata:create metadata-file-name)
        (org-glance-overview:create-archive tag)
        (with-temp-file overview-file-name
          (org-glance-overview:refresh-widgets tag))))
    overview-file-name))

(declare-function org-glance-overview-v2 "org-glance-overview-v2" (&optional tag))
(defvar org-glance-use-graph-v2)

(cl-defun org-glance-overview (&optional tag)
  "Open the overview.
Called with NO TAG (interactive) and `org-glance-use-graph-v2' enabled, browse
the v2 graph: prompt for a tag (empty input = all) and show the overview
filtered by it, cached per tag.  Called WITH a TAG (e.g. the
`org-glance-overview:' link handler), use the v1 per-tag overview file."
  (interactive)
  (cond ((and org-glance-use-graph-v2 (null tag))
         (call-interactively #'org-glance-overview-v2))
        (t
         (let ((tag (or tag (org-glance-tags:completing-read "Overview: " nil))))
           (cl-check-type tag org-glance-tag)
           (when-let (location (org-glance-overview:file-name tag))
             (if (file-exists-p location)
                 (find-file location)
               (org-glance:create-tag tag)
               (org-glance-overview:create tag)))))))

(cl-defun org-glance-overview:agenda ()
  (interactive)
  (let ((org-agenda-start-on-weekday nil)
        (org-agenda-overriding-header "org-glance agenda")
        (org-agenda-files (list (buffer-file-name))))
    (save-window-excursion
      (org-agenda-list nil "-7d" 21))
    (switch-to-buffer org-agenda-buffer)))

(cl-defun org-glance-overview:agenda* ()
  (interactive)
  (let ((org-agenda-overriding-header "org-glance agenda")
        (org-agenda-start-on-weekday nil)
        (org-agenda-files (mapcar 'org-glance-overview:file-name (org-glance:tags-sorted))))
    (save-window-excursion
      (org-agenda-list nil "-2d" 7))
    (switch-to-buffer org-agenda-buffer)))

(cl-defun org-glance-overview:materialize-headline ()
  (let ((headline (org-glance-overview:original-headline)))
    (switch-to-buffer (org-glance-headline:materialize headline))))

(cl-defun org-glance-overview:jump-headline ()
  (interactive)
  (let ((offset (- (point) (save-excursion
                             (org-glance-headline:search-parents)
                             (point)))))
    (-some->> (org-glance-headline:at-point)
      org-glance-headline:id
      org-glance-metadata:headline-metadata
      org-glance:open)
    (forward-char offset)))

(cl-defun org-glance-overview:category ()
  (org-entry-get-with-inheritance "CATEGORY" nil 0))

(cl-defun org-glance-overview:tag ()
  "Return tag name of current overview."
  (save-excursion
    (goto-char (point-min))
    (let ((tag (org-glance-tag:from-string (org-glance-overview:category))))
      (when (gethash tag org-glance-tags)
        tag))))

(cl-defun org-glance-overview:kill-headline (&key (force nil))
  "Remove `org-glance-headline' from overview, don't ask to confirm if FORCE is t."
  (interactive)
  (org-glance-headline:search-parents)
  (let ((title (org-glance-headline:plain-title))
        (tag (org-glance-overview:tag))
        (original-headline (org-glance-overview:original-headline)))
    (when (or force (y-or-n-p (format "Revoke the tag \"%s\" from \"%s\"?" tag title)))
      (save-window-excursion
        (org-glance:with-headline-materialized original-headline
          (cl-loop with tags = (org-glance-tag:from-headline-at-point)
                   with indices = (--find-indices (eq tag it) tags)
                   for index in indices
                   do (org-toggle-tag (nth index tags) 'off)))))))

(cl-defun org-glance-overview:pull ()
  "Pull any modifications from original headline to it's overview at point."
  (interactive)
  (let* ((inhibit-read-only t)
         (initial-point (point))
         (current-headline (org-glance-headline:at-point))
         (current-headline-title (org-glance-headline:plain-title current-headline))
         (current-headline-contents (org-glance-headline:contents current-headline))
         (original-headline (org-glance-overview:original-headline))
         (overview-contents (org-glance-headline:overview original-headline)))
    (cond ((null overview-contents)
           (if (y-or-n-p (format "Original headline for \"%s\" not found. Remove it from overview?" current-headline-title))
               (org-glance-overview:kill-headline :force t)
             (org-glance-headline:not-found! "Original headline not found"))
           nil)
          ((string= current-headline-contents overview-contents)
           (message (format "Headline \"%s\" is up to date" current-headline-title))
           t)
          (t (org-glance-headline:replace-headline-at-point overview-contents)
             (org-overview)
             (goto-char initial-point)
             (org-align-tags t)
             (condition-case nil
                 (org-update-checkbox-count-maybe)
               (error nil))
             (save-buffer)
             (message (format "Headline \"%s\" is now up to date" current-headline-title))
             t))))

(cl-defun org-glance-overview:archive ()
  "Archive headline at point."
  (interactive)
  (save-window-excursion
    (org-glance:with-headline-materialized (org-glance-overview:original-headline)
      (org-toggle-archive-tag))))

(cl-defun org-glance-overview:move ()
  "Move headline to another tag."
  (interactive)
  (let* ((old-tag (org-glance-overview:tag))
         (new-tag (let ((tages (--filter (not (eql old-tag it)) (org-glance:tags-sorted))))
                    (intern (completing-read "Move headline to: " tages nil t))))
         (original-headline (org-glance-overview:original-headline)))
    (org-glance:create-tag new-tag)
    (save-window-excursion
      (org-glance:with-headline-materialized original-headline
        (cl-loop with tags = (org-get-tags)
                 with indices = (--find-indices (eql old-tag (org-glance-tag:from-string it)) tags)
                 for index in indices
                 do (org-toggle-tag (nth index tags) 'off)
                 finally (org-toggle-tag (symbol-name new-tag) 'on))))))

(cl-defun org-glance-overview:add-tag ()
  "Add tag to headline."
  (interactive)
  (let* ((original-headline (org-glance-overview:original-headline))
         (old-tages (org-glance-headline:tags original-headline))
         (new-tag (let ((views (--filter (not (member it old-tages)) (org-glance:tags-sorted))))
                      (intern (completing-read "Add tag: " views)))))
    (save-window-excursion
      (org-glance:with-headline-materialized original-headline
        (org-toggle-tag (symbol-name new-tag) 'on)))))

(cl-defun org-glance-overview:original-headline ()
  (->> (org-glance-headline:at-point)
       org-glance-headline:id
       org-glance-metadata:headline))

(cl-defun org-glance-overview:beginning-of-headlines ()
  (save-excursion
    (goto-char (point-min))
    (outline-next-heading)
    (point)))

(cl-defun org-glance-overview:order (&optional (tag (org-glance-overview:tag)))
  (interactive)
  (cl-check-type tag org-glance-tag)

  (save-excursion
    (let ((inhibit-read-only t)
          (beginning-of-headlines (org-glance-overview:beginning-of-headlines))
          (end-of-headlines (point-max)))

      (cl-loop for buffer in (org-glance-overview:partition
                               :using (-partial #'org-glance-overview:partition-mapper tag)
                               :comparator #'org-glance-overview:partition-comparator)
               do
               (goto-char (point-max))
               (insert (let ((standard-output 'ignore))
                         (with-current-buffer buffer
                           (set-mark (point-min))
                           (goto-char (point-max))
                           (org-sort-entries nil ?a)
                           (buffer-substring-no-properties (point-min) (point-max)))))
               (kill-buffer buffer))
      (delete-region beginning-of-headlines end-of-headlines)
      (org-overview)
      (save-buffer))))

;; (cl-defun org-glance-overview:vizualize ()
;;   (interactive)
;;   (org-glance-overview:apply-to-buffer-headlines
;;       (error "not implemented yet")
;;     (let ((relations (org-glance-headline-relations)))
;;       (with-temp-file "relations.js"
;;         (insert "var relations = ["
;;                 (s-join "," (cl-loop
;;                                for rel in relations
;;                                for name = (car rel)
;;                                for relations = (s-join "," (mapcar (-rpartial #'s-wrap "\"") (cdr rel)))
;;                                collect (org-glance:format "{\"name\":\"${name}\",\"relations\":[${relations}]}")))
;;                 "];")))))

;; (cl-defun -og-calw-d (day)
;;   (let ((org-agenda-files (mapcar 'org-glance-overview:file-name (org-glance:tags-sorted))))
;;     (org-agenda-list)
;;     (org-agenda-day-view day)
;;     (switch-to-buffer org-agenda-buffer)
;;     (delete-other-windows)))

(provide 'org-glance-overview)
