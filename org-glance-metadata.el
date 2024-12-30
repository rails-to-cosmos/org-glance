;; -*- lexical-binding: t -*-

(require 'org-glance-exception)
(require 'org-glance-headline)
(require 'org-glance-tag)

(org-glance-exception:define org-glance-metadata-!-outdated
  "Headline metadata is outdated")

(cl-defun org-glance-metadata:read-tag-metadata (tag)
  (->> tag
       org-glance-metadata:location
       org-glance-metadata:read))

(cl-defun org-glance-metadata:get-headline (id)
  (cl-check-type id string)

  (cl-loop for tag being the hash-keys of org-glance-tags
           for metadata = (org-glance-metadata:read-tag-metadata tag)
           for headline = (gethash id metadata) ;; TODO use symbols instead of strings
           when headline
           collect (org-glance-headline:deserialize id headline)
           into result
           finally (return (car result))))

;; TODO refactor, slow
(cl-defun org-glance-metadata:choose-headline (&key (filter #'org-glance-headline:active?))
  "Main retriever, refactor it."
  (let* ((headers (org-glance-metadata:read-headers filter))
         (choice (completing-read "Headline: " headers nil t)))
    (cl-destructuring-bind (header tag)
        (alist-get choice headers nil nil #'string=)
      (org-glance-headline:with-narrowed-headline header
        (org-glance-headline:update (org-glance-headline:at-point) :tag tag)))))

(cl-defun org-glance-metadata:headlines (metadata)
  (cl-loop for id being the hash-keys of metadata using (hash-value value)
           collect (org-glance-headline:deserialize id value)))

(cl-defun org-glance-metadata:read (file)
  (with-temp-buffer
    (insert-file-contents file)
    (read (buffer-substring-no-properties (point-min) (point-max)))))

(cl-defun org-glance-metadata:save (metadata file)
  (declare (indent 1))
  (mkdir (file-name-directory file) 'parents)
  (with-temp-file file
    (insert (prin1-to-string metadata)))
  metadata)

(cl-defun org-glance-metadata:add-headline (headline metadata)
  (let ((id (org-glance-headline:id headline)))
    (puthash id (org-glance-headline:serialize headline) metadata)))

(cl-defun org-glance-metadata:remove-headline (headline metadata)
  (remhash (org-glance-headline:id headline) metadata))

(cl-defun org-glance-metadata:create (file &optional headlines)
  "Create metadata from HEADLINES and write it to FILE."
  (unless (f-exists? file)
    (cl-loop with metadata = (make-hash-table :test 'equal)
             for headline in headlines
             do (org-glance-metadata:add-headline headline metadata)
             finally (org-glance-metadata:save metadata file))))

(cl-defun org-glance-metadata:location (tag)
  (format "%s/%s/%s.metadata.el" org-glance-directory tag tag))

(cl-defun org-glance-headline-reference (&optional (type 'org-glance-visit))
  (org-glance-headline:with-headline-at-point
   (let* ((headline (org-glance-headline:at-point))
          (id (org-glance-headline:id headline))
          (state (org-glance-headline:state headline))
          (alias (-some->> (org-glance:headline-alias)
                   (replace-regexp-in-string (format "^%s[[:space:]]*" state) "")))
          (tags (org-glance-headline:tags headline))
          (tag (s-join ", " (cl-loop for tag in tags
                                     collect (format "[[org-glance-overview:%s][%s]]" tag tag)))))
     (format "%s%s [[%s:%s][%s]]"
             (if (string-empty-p state) "" (format "[[org-glance-state:%s][%s]] " state state))
             tag
             type
             id
             alias))))

(cl-defun org-glance-relation-interpreter (relation)
  ;; please, avoid metadata here
  (org-element-link-interpreter (org-element-property :link relation)
                                (org-element-property :contents relation))

  ;; (org-glance-headline-reference)
  ;; (org-glance-headline:with-narrowed-headline (org-glance-metadata:get-headline (org-element-property :id relation))
  ;;   (let ((ref )
  ;;         (ts (cl-loop for timestamp in (-some->> (org-glance-datetime-headline-timestamps 'include-schedules)
  ;;                                         (org-glance-datetime-filter-active)
  ;;                                         (org-glance-datetime-sort-timestamps))
  ;;                collect (org-element-property :raw-value timestamp))))
  ;;     (if ts
  ;;         (concat ref " on " (car ts))
  ;;       ref)))
  )

(cl-defun org-glance-relation-type-parser ()
  (quote mention))

(cl-defun org-glance-relation-type-interpreter (relation)
  (cl-case (org-element-property :type relation)
    ((subtask quote) "Subtask")
    ((project quote) "Part of a project")
    ((mention quote) "-")
    (t (prin1-to-string (org-element-property :type relation)))))

(cl-defun org-glance-headline-relations ()
  "Get all first-level relations of headline at point."
  (org-glance-headline:with-headline-at-point
   (cl-loop with relations = (make-hash-table)
            for link in (org-element-map (org-element-parse-buffer) 'link #'identity)
            for id = (intern (org-element-property :path link))
            for type = (cond
                        ((memq (intern (org-element-property :type link)) '(org-glance-visit org-glance-open))
                         (let ((link-type (save-excursion
                                            (goto-char (org-element-property :begin link))
                                            (org-glance-relation-type-parser))))
                           (if (memq link-type '(subtask subtask-done project project-done))
                               ;; actualize link state for subtasks and projects
                               (if-let (headline (org-glance-metadata:get-headline id))
                                   (condition-case nil
                                       (org-glance-headline:with-narrowed-headline headline
                                         (let ((state (intern (or (org-get-todo-state) "")))
                                               (done-kws (mapcar #'intern org-done-keywords)))
                                           (cond ((memq state done-kws) (cond ((memq link-type '(subtask subtask-done)) 'subtask-done)
                                                                              ((memq link-type '(project project-done)) 'project-done)
                                                                              (t 'subtask-done)))
                                                 (t (cond ((memq link-type '(subtask subtask-done)) 'subtask)
                                                          ((memq link-type '(project project-done)) 'project)
                                                          (t 'subtask))))))
                                     (org-glance-headline-!-not-found link-type))
                                 link-type)
                             link-type)))
                        (t nil))
            when (and type
                      (or (not (gethash id relations))
                          (and (memq type '(subtask subtask-done))
                               (not (memq (org-element-property :type (gethash id relations))
                                          '(subtask subtask-done))))))
            do (let ((relation (list 'org-glance-relation
                                     (list :id id
                                           :type type
                                           :contents (condition-case nil
                                                         (buffer-substring-no-properties
                                                          (org-element-property :contents-begin link)
                                                          (org-element-property :contents-end link))
                                                       (error nil))
                                           :link link))))
                 (puthash id relation relations))
            finally (return (hash-table-values relations)))))

(provide 'org-glance-metadata)
