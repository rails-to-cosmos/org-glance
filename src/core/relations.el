(require 'org-glance-module)

(cl-defun org-glance-relation-interpreter (relation)
  ;; please, avoid metastore here
  (org-element-link-interpreter (org-element-property :link relation)
                                (org-element-property :contents relation))

  ;; (org-glance-headline-reference)
  ;; (org-glance:with-headline-narrowed (org-glance-metastore:get-headline (org-element-property :id relation))
  ;;   (let ((ref )
  ;;         (ts (cl-loop for timestamp in (-some->> (org-glance-ts-headline-timestamps 'include-schedules)
  ;;                                         (org-glance-ts-filter-active)
  ;;                                         (org-glance-ts-sort-timestamps))
  ;;                collect (org-element-property :raw-value timestamp))))
  ;;     (if ts
  ;;         (concat ref " on " (car ts))
  ;;       ref)))
  )

(cl-defun org-glance-relation-type-parser ()
  'mention)

(cl-defun org-glance-relation-type-interpreter (relation)
  (case (org-element-property :type relation)
    ('subtask "Subtask")
    ('project "Part of a project")
    ('mention "-")
    (t (prin1-to-string (org-element-property :type relation)))))

(cl-defun org-glance-headline-relations ()
  "Get all first-level relations of headline at point."
  (org-glance:with-headline-at-point
   (cl-loop
      with relations = (make-hash-table)
      for link in (org-element-map (org-element-parse-buffer) 'link #'identity)
      for id = (intern (org-element-property :path link))
      for type = (cond
                   ((memq (intern (org-element-property :type link)) '(org-glance-visit org-glance-open))
                    (let ((link-type (save-excursion
                                       (goto-char (org-element-property :begin link))
                                       (org-glance-relation-type-parser))))
                      (if (memq link-type '(subtask subtask-done project project-done))
                          ;; actualize link state for subtasks and projects
                          (if-let (headline (org-glance-metastore:get-headline id))
                              (condition-case nil
                                  (org-glance:with-headline-narrowed headline
                                    (let ((state (intern (or (org-get-todo-state) "")))
                                          (done-kws (mapcar #'intern org-done-keywords)))
                                      (cond ((memq state done-kws) (cond ((memq link-type '(subtask subtask-done)) 'subtask-done)
                                                                         ((memq link-type '(project project-done)) 'project-done)
                                                                         (t 'subtask-done)))
                                            (t (cond ((memq link-type '(subtask subtask-done)) 'subtask)
                                                     ((memq link-type '(project project-done)) 'project)
                                                     (t 'subtask))))))
                                (org-glance-exception:HEADLINE-NOT-FOUND link-type))
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

(org-glance:provide)
