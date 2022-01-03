(require 'org-glance-module)

(org-glance:require
  org
  org-element
  lib.core.exceptions
  lib.utils.helpers)

(cl-defun org-glance-headline-p (&optional (headline (org-element-at-point)))
  "Assume HEADLINE is an `org-element' with :ORG_GLANCE_ID property specified.
Return headline or nil if it is not a proper `org-glance-headline'."
  (when (org-element-property :ORG_GLANCE_ID headline)
    headline))

(defvar org-glance-headline:serde-alist nil
  "Map `org-element-property' to `org-glance' extractor method.

It is safe (in terms of backward/forward compability of
metastores) to append properties to this map.

Do not modify existing properties without backfilling of
metastore.")

(setq org-glance-headline:serde-alist
      `((:raw-value  . (:reader org-glance-headline:title      :writer org-glance-headline:title))
        (:begin      . (:reader org-glance-headline:begin      :writer org-glance-headline:begin))
        (:file       . (:reader org-glance-headline:file       :writer org-glance-headline:file))
        (:commentedp . (:reader org-glance-headline:commented? :writer org-glance-headline:commented?))
        (:archivedp  . (:reader org-glance-headline:archived?  :writer org-glance-headline:archived?))
        (:contains-link-p    . (:reader org-glance-headline:contains-link?    :writer (lambda (hl)
                                                                                        (save-excursion
                                                                                          (save-restriction
                                                                                            (org-narrow-to-subtree)
                                                                                            ;; (org-end-of-meta-data t)
                                                                                            (when (org-glance-buffer-links)
                                                                                              'contains-link))))))
        (:contains-property-p        . (:reader org-glance-headline:contains-property?       :writer (lambda (hl)
                                                                                                       (save-excursion
                                                                                                         (save-restriction
                                                                                                           (org-narrow-to-subtree)
                                                                                                           (org-end-of-meta-data t)
                                                                                                           (when (re-search-forward org-glance:key-value-pair-re nil t)
                                                                                                             'contains-properties))))))
        (:encryptedp . (:reader org-glance-headline:encrypted-p :writer (lambda (hl)
                                                                         (save-excursion
                                                                           (org-end-of-meta-data t)
                                                                           (when (looking-at "aes-encrypted V [0-9]+.[0-9]+-.+\n")
                                                                             'encrypted)))))
        (:buffer . (:reader org-glance-headline:buffer :writer (lambda (hl)
                                                                 (condition-case nil
                                                                     (buffer-name (get-file-buffer (org-glance-headline:file hl)))
                                                                   (wrong-type-argument (buffer-name))))))))

(cl-defun org-glance-headline:serialize (headline)
  "Serialize HEADLINE to store it on disk."
  (cl-loop
     for (property . methods) in org-glance-headline:serde-alist
     collect (funcall (plist-get methods :reader) headline)))

(cl-defun org-glance-headline:deserialize (dump)
  "Deserialize DUMP to minimal headline."
  (cl-loop
     with element = (org-element-create 'headline)
     for (property . _) in org-glance-headline:serde-alist
     for index from 0
     do (org-glance-headline:enrich element property (nth index dump))
     finally (return element)))

(cl-defun org-glance-headline:enrich (element &rest kwargs)
  "Enrich `org-element' ELEMENT with KWARGS properties."
  (declare (indent 1))
  (cl-loop
     for (key value) on kwargs by #'cddr
     do (org-element-put-property element key value)
     finally (return element)))

(cl-defun org-glance-headline:create-from-element-at-point ()
  "Create `org-glance-headline' from element at point."
  (let ((prototype (org-element-at-point)))
    (when (eql 'headline (org-element-type prototype))
      (cl-loop
         for (property . methods) in org-glance-headline:serde-alist
         for index from 0
         do (org-glance-headline:enrich prototype property (funcall (plist-get methods :writer) prototype))
         finally (return prototype)))))

(cl-defun org-glance-headline:search-parents ()
  "Traverse parents in search of a proper `org-glance-headline'."
  (org-glance-ensure-at-heading)
  (beginning-of-line)
  (while (and (not (org-glance-headline-p))
              (not (org-before-first-heading-p)))
    (org-up-heading-or-point-min))
  (org-glance-headline:create-from-element-at-point))

(cl-defun org-glance-headline:at-point ()
  "Search for the first occurence of `org-glance-headline' in parent headlines."
  (save-excursion
    (org-glance-headline:search-parents)))

(cl-defun org-glance-headline:active? (&optional (headline (org-element-at-point)))
  (and (org-glance-headline-p headline)
       (not (org-glance-headline:commented? headline))
       (not (org-glance-headline:archived? headline))))

(cl-defun org-glance-headline:search-backward ()
  (interactive)
  (while (and (outline-previous-heading) (not (org-glance-headline:active?))))
  (when (org-glance-headline:active?) (org-glance-headline:at-point)))

(cl-defun org-glance-headline:search-forward ()
  (interactive)
  (while (and (outline-next-heading) (not (org-glance-headline:active?))))
  (when (org-glance-headline:active?) (org-glance-headline:at-point)))

(cl-defun org-glance-headline:id (&optional (headline (org-glance-headline:at-point)))
  "Return unique identifer of HEADLINE."
  (org-element-property :ORG_GLANCE_ID headline))

(cl-defun org-glance-headline:state (&optional (headline (org-glance-headline:at-point)))
  (substring-no-properties (or (org-element-property :todo-keyword headline) "")))

(cl-defun org-glance-headline:commented? (&optional (headline (org-glance-headline:at-point)))
  (when (org-element-property :commentedp headline)
    t))

(cl-defun org-glance-headline:archived? (&optional (headline (org-glance-headline:at-point)))
  (when (org-element-property :archivedp headline)
    t))

(cl-defun org-glance-headline:raw-value (&optional (headline (org-glance-headline:at-point)))
  (org-element-property :raw-value headline))

(cl-defun org-glance-headline:title (&optional (headline (org-glance-headline:at-point)))
  "Get title of HEADLINE, cleanup links."
  (with-temp-buffer
    (insert (or (org-element-property :TITLE headline)
                (org-element-property :raw-value headline)
                ""))
    (cl-loop
       for (title beg end) in (org-element-map (org-element-parse-buffer) 'link
                                (lambda (link) (list
                                                (substring-no-properties
                                                 (or (nth 2 link)
                                                     (org-element-property :raw-link link)))
                                                (org-element-property :begin link)
                                                (org-element-property :end link))))
       collect title into titles
       collect (s-trim (buffer-substring-no-properties beg end)) into links
       finally (return (cl-loop
                          initially (goto-char (point-min))
                          for i upto (1- (length titles))
                          do (replace-string (nth i links) (nth i titles))
                          finally (return (buffer-substring-no-properties (point-min) (point-max))))))))

(cl-defun org-glance-headline:priority (&optional (headline (org-glance-headline:at-point)))
  (org-element-property :priority headline))

(cl-defun org-glance-headline:creation-time (&optional (headline (org-glance-headline:at-point)))
  (org-element-property :ORG_GLANCE_CREATION_TIME headline))

(cl-defun org-glance-headline:modtime (&optional (headline (org-glance-headline:at-point)))
  (-some-> headline
    org-glance-headline:file
    file-attributes
    file-attribute-modification-time
    (format-time-string "%Y-%m-%d %H:%M:%S")))

(cl-defun org-glance-headline:file (&optional (headline (org-glance-headline:at-point)))
  (when-let (file (if (plist-member (nth 1 headline) :file)
                      (org-element-property :file headline)
                    (buffer-file-name)))
    (abbreviate-file-name file)))

(cl-defun org-glance-headline:level (&optional (headline (org-glance-headline:at-point)))
  (org-element-property :level headline))

(cl-defun org-glance-headline:buffer (&optional (headline (org-glance-headline:at-point)))
  (let ((buffer (org-element-property :buffer headline)))
    (cond ((bufferp buffer) (buffer-name buffer))
          (t buffer))))

(cl-defun org-glance-headline:begin (&optional (headline (org-glance-headline:at-point)))
  (org-element-property :begin headline))

(cl-defun org-glance-headline:tags (&optional (headline (org-glance-headline:at-point)))
  (mapcar #'s-titleized-words (org-element-property :tags headline)))

(cl-defun org-glance-headline:search-buffer-by-id (id)
  (org-glance:log-debug "I'm in buffer \"%s\"" (current-buffer))
  (let ((points (org-element-map (org-element-parse-buffer 'headline) 'headline
                  (lambda (el) (when (string= (org-glance-headline:id el) id)
                              (org-element-property :begin el))))))
    (unless points
      (org-glance-exception:HEADLINE-NOT-FOUND "Headline not found in file %s: %s" (buffer-file-name) id))
    (when (> (length points) 1)
      (org-glance:log-warning "Headline ID %s is not unique in file %s" id (buffer-file-name)))
    (goto-char (car points))
    (org-glance-headline:at-point)))

(cl-defun org-glance-headline:visit (&optional (headline (org-glance-headline:at-point)))
  "Visit HEADLINE."
  (let* ((id (org-glance-headline:id headline))
         (file (org-glance-headline:file headline))
         (buffer (org-glance-headline:buffer headline))
         (revert-without-query (list file)))

    (cond ((and file (file-exists-p file)) (find-file file))
          ((and buffer (buffer-live-p buffer)) (switch-to-buffer buffer))
          (t (org-glance:log-warning "File and buffer not found for visiting. Using current buffer...")))

    (org-glance:log-debug "We are now visiting headline buffer %s, let's remove restrictions" (current-buffer))
    (widen)
    (org-glance:log-debug "Search headline in buffer")
    (cond (id (org-glance-headline:search-buffer-by-id id))
          (t (goto-char (org-glance-headline:begin headline))))))

(cl-defmacro org-glance-headline:narrow (headline &rest forms)
  "Visit HEADLINE, narrow to its subtree and execute FORMS on it."
  (declare (indent 1) (debug t))
  `(save-window-excursion
     (save-excursion
       (save-restriction
         (org-glance-headline:visit ,headline)
         (org-narrow-to-subtree)
         ,@forms))))

(cl-defun org-glance-headline:promote-to-the-first-level ()
  (org-glance-ensure-at-heading)
  (while (and (org-glance-headline-p) (looking-at "^\\*\\*"))
    (org-promote-subtree)))

(cl-defun org-glance-headline:replace-headline-at-point (new-contents)
  (let ((beg (org-glance-headline:begin))
        (end (save-excursion (org-end-of-subtree t)))
        (inhibit-read-only t))
    (delete-region beg end)
    (goto-char beg)
    (insert new-contents)))

(cl-defun org-glance-headline-contents (&optional (headline (org-glance-headline:at-point)))
  "Extracts HEADLINE contents.
FIXME. Unstable one. Refactor is needed."
  (let ((file (org-glance-headline:file headline))
        (buffer (org-glance-headline:buffer headline)))
    (cond (file (with-temp-buffer
                  (org-glance:log-debug "Extract contents for headline %s from file %s" (org-glance-headline:id headline) file)
                  (org-mode)
                  (insert-file-contents file)
                  (org-glance-headline:search-buffer-by-id (org-glance-headline:id headline))
                  (org-narrow-to-subtree)
                  (goto-char (point-min))
                  (org-glance-headline:promote-to-the-first-level)
                  (s-trim (buffer-substring-no-properties (point-min) (point-max)))))
          (buffer (with-current-buffer buffer
                    (org-glance:log-debug "Extract contents for headline %s from buffer %s" (org-glance-headline:id headline) buffer)
                    (save-window-excursion
                      (save-excursion
                        (save-restriction
                          (widen)
                          (org-glance-headline:search-buffer-by-id (org-glance-headline:id headline))
                          (org-narrow-to-subtree)
                          (let ((contents (s-trim (buffer-substring-no-properties (point-min) (point-max)))))
                            (with-temp-buffer
                              (org-mode)
                              (insert contents)
                              (goto-char (point-min))
                              (outline-next-heading)
                              (org-glance-headline:promote-to-the-first-level)
                              (s-trim (buffer-substring-no-properties (point-min) (point-max))))))))))
          (t (org-glance-exception:HEADLINE-NOT-FOUND "Unable to determine headline location")))))

(cl-defgeneric org-glance-headline:extract-from (scope)
  "Extract `org-glance-headlines' from scope.")

(cl-defmethod org-glance-headline:extract-from ((f string))
  "Extract headlines from file F."
  (if-let (b (get-buffer f)) ;; buffer name
      (org-glance-headline:extract-from b)
    (with-temp-buffer
      (org-glance:log-debug "Scan file %s" f)
      (insert-file-contents f)
      (org-mode)
      (cl-loop
         for headline in (org-glance-headline:extract-from (current-buffer))
         collect (org-glance-headline:enrich headline :file (abbreviate-file-name f))))))

(cl-defmethod org-glance-headline:extract-from ((b buffer))
  "Extract headlines from buffer B."
  (with-current-buffer b
    (org-element-map (org-element-parse-buffer 'headline) 'headline
      (lambda (e)
        (when (org-glance-headline-p e)
          (save-excursion
            (goto-char (org-glance-headline:begin e))
            (org-glance-headline:enrich (org-glance-headline:create-from-element-at-point)
              :buffer b)))))))

(cl-defun org-glance-headline:add-log-note (string &rest objects)
  (save-excursion
    (org-glance-ensure-at-heading)
    (goto-char (org-log-beginning t))
    (insert (apply #'format string objects) "\n")))

(cl-defmacro org-glance-headline:format (headline)
  (declare (indent 1) (debug t))
  `(let* ((id (org-glance-headline:id ,headline))
          (state (org-glance-headline:state ,headline))
          ;; (state-label (if (string-empty-p state) "" (format "*%s*" state)))
          (title (org-glance-headline:title ,headline))
          (stateless-title (replace-regexp-in-string (format "^%s[[:space:]]*" state) "" title))
          ;; (category (if-let (category (org-element-property :CATEGORY headline))
          ;;               (downcase (format "=%s= " category))
          ;;             ""))
          (now (format-time-string (org-time-stamp-format 'long 'inactive) (current-time))))
     (s-trim
      (org-glance:format "[[org-glance-visit:${id}][${stateless-title}]]"))))

(cl-defun org-glance-headline:encrypt (&optional password)
  "Encrypt subtree at point with PASSWORD."
  (interactive)
  (let ((beg (save-excursion (org-end-of-meta-data t) (point)))
        (end (save-excursion (org-end-of-subtree t) (point))))
    (org-glance:encrypt-region beg end password)))

(cl-defun org-glance-headline:decrypt (&optional password)
  "Decrypt subtree at point with PASSWORD."
  (interactive)
  (let ((beg (save-excursion (org-end-of-meta-data t) (point)))
        (end (save-excursion (org-end-of-subtree t) (point))))
    (org-glance:decrypt-region beg end password)))

(cl-defun org-glance-headline:demote (level)
  (cl-loop repeat level
     do (org-with-limited-levels
         (org-map-tree 'org-demote))))

;; Relations

(cl-defun org-glance-relation-interpreter (relation)
  (concat
   (org-glance-relation-type-interpreter relation)
   (org-element-link-interpreter (org-element-property :link relation)
                                 (org-element-property :contents relation))))

(cl-defun org-glance-relation-type-parser ()
  (cond ((and (org-at-item-checkbox-p) (looking-back "- \\[ \\] " 6)) 'subtask)
        ((and (org-at-item-checkbox-p) (looking-back "- \\[X\\] " 6)) 'subtask-done)
        ((and (org-at-item-p) (looking-back "- \\[ \\] Part of a project " 25)) 'project)
        ((and (org-at-item-p) (looking-back "- \\[X\\] Part of a project " 25)) 'project-done)
        (t 'mention)))

(cl-defun org-glance-relation-type-interpreter (relation)
  (concat
   (case (org-element-property :type relation)
     ('subtask "- [ ]")
     ('subtask-done "- [X]")
     ('project "- [ ] Part of a project")
     ('project-done "- [X] Part of a project")
     ('mention "-")
     (t (prin1-to-string (org-element-property :type relation))))
   " "))

(cl-defun org-glance-headline-relations ()
  "Get all first-level relations of headline at point."
  (org-glance-headline:with-headline-at-point
   (cl-loop
      with relations = (make-hash-table)
      for link in (org-element-map (org-element-parse-buffer) 'link #'identity)
      for id = (intern (org-element-property :path link))
      for type = (cond
                   ((s-contains? "org-glance" (org-element-property :type link))
                    (let ((link-type (save-excursion
                                       (goto-char (org-element-property :begin link))
                                       (org-glance-relation-type-parser))))
                      (if (memq link-type '(subtask subtask-done project project-done))
                          ;; actualize link state for subtasks and projects
                          (condition-case nil
                              (save-window-excursion
                                (org-glance-headline:with-materialized-headline (org-glance-metastore:get-headline (symbol-name id))
                                  (let ((state (intern (or (org-get-todo-state) "")))
                                        (done-kws (mapcar #'intern org-done-keywords)))
                                    (cond ((memq state done-kws) (cond ((memq link-type '(subtask subtask-done)) 'subtask-done)
                                                                       ((memq link-type '(project project-done)) 'project-done)
                                                                       (t 'subtask-done)))
                                          (t (cond ((memq link-type '(subtask subtask-done)) 'subtask)
                                                   ((memq link-type '(project project-done)) 'project)
                                                   (t 'subtask)))))))
                            (org-glance-exception:HEADLINE-NOT-FOUND link-type))
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

(cl-defun org-glance-headline-relation:subtask-p (relation)
  (memq (org-element-property :type relation) '(subtask subtask-done)))

(cl-defun org-glance-headline:subtasks ()
  (cl-loop
     for relation in (org-glance-headline-relations)
     when (org-glance-headline-relation:subtask-p relation)
     collect relation))

(cl-defun org-glance-headline:hash (&optional (headline (org-glance-headline:at-point)))
  (let ((contents (org-glance-headline-contents headline)))
    (with-temp-buffer
      (org-mode)
      (insert contents)
      (buffer-hash))))

(cl-defun org-glance-headline:contains-property? (&optional (headline (org-glance-headline:at-point)))
  (when (org-element-property :contains-property-p headline)
    'contains-property))

(cl-defun org-glance-headline:contains-link? (&optional (headline (org-glance-headline:at-point)))
  (when (org-element-property :contains-link-p headline)
    'contains-link))

(cl-defun org-glance-headline:encrypted-p (&optional (headline (org-glance-headline:at-point)))
  (when (org-element-property :encryptedp headline)
    'encrypted))

(cl-defun org-glance-headline:string-to-class (tag)
  (intern (s-downcase tag)))

(cl-defun org-glance-headline:classes (&optional (headline (org-glance-headline:at-point)))
  (cl-loop
     for tag in (org-element-property :tags headline)
     collect (org-glance-headline:string-to-class tag)))

(cl-defun org-glance-headline:scheduled (&optional (headline (org-glance-headline:at-point)))
  (org-element-property :scheduled headline))

(cl-defun org-glance-headline:deadline (&optional (headline (org-glance-headline:at-point)))
  (org-element-property :deadline headline))

(cl-defun org-glance-headline:repetitive-p ()
  (org-glance-headline:with-headline-at-point
   (when (append
          (-some->> (org-tss:subtree-timestamps 'include-schedules 'include-deadlines)
            (org-tss:filter-active)
            (org-tss:filter-repetitive)
            (org-tss:sort)))
     t)))

(cl-defun org-glance-headline:generate-directory (location title)
  (abbreviate-file-name
   (make-temp-file
    (-org-glance:make-file-directory
     (f-join location
             (concat (format-time-string "%Y-%m-%d_")
                     (->> title
                          (replace-regexp-in-string "[^a-z0-9A-Z_]" "-")
                          (replace-regexp-in-string "\\-+" "-")
                          (replace-regexp-in-string "\\-+$" "")
                          (s-truncate 30))
                     "-")))
    'directory)))

(cl-defun org-glance-headline:overview (&optional (headline (org-glance-headline:at-point)))
  "Return HEADLINE high-level usability characteristics."
  (save-window-excursion
    (org-glance-headline:visit headline)
    (org-glance-headline:with-headline-at-point
     (let ((timestamps (cl-loop for timestamp in (-some->> (org-tss:subtree-timestamps)
                                                   (org-tss:filter-active)
                                                   (org-tss:sort))
                          collect (org-element-property :raw-value timestamp)))
           (header (save-excursion
                     (goto-char (point-min))
                     (org-end-of-meta-data)
                     (s-trim (buffer-substring-no-properties (point-min) (point)))))
           (relations (cl-loop
                         for relation in (org-glance-headline-relations)
                         when (eq (org-element-property :type relation) 'mention)
                         collect relation into mentions
                         when (memq (org-element-property :type relation) '(subtask subtask-done))
                         collect relation into subtasks
                         when (memq (org-element-property :type relation) '(project project-done))
                         collect relation into projects
                         finally (return (list :mentions mentions
                                               :subtasks subtasks
                                               :projects projects))))
           (tags (org-get-tags-string))
           (state (org-glance-headline:state headline))
           (id (org-glance-headline:id headline))
           (title (org-glance-headline:title headline))
           (priority (org-glance-headline:priority headline))
           (schedule (org-glance-headline:scheduled headline))
           (deadline (org-glance-headline:deadline headline))
           (encrypted (org-glance-headline:encrypted-p))
           (repetitive (org-glance-headline:repetitive-p))
           (linked (org-glance-headline:contains-link?)))
       (with-temp-buffer
         (insert
          (concat
           "* "
           state
           (if (string-empty-p state)
               ""
             " ")
           (if priority
               (concat "[#" (char-to-string priority) "]" " ")
             "")
           title
           ;; " "
           ;; tags
           "\n"
           (if schedule
               (concat "SCHEDULED: " (org-element-property :raw-value schedule))
             "")
           (if deadline
               (concat "DEADLINE: " (org-element-property :raw-value deadline))
             "")
           (if (or schedule deadline)
               "\n"
             "")

           ":PROPERTIES:\n"
           ":ORG_GLANCE_ID: " id "\n"
           ":DIR: " (abbreviate-file-name default-directory) "\n"
           ":END:"

           (org-glance-join-but-null "\n\n"
                                     (list
                                      (when (or encrypted linked repetitive)
                                        (concat "- Usability characteristics"
                                                (org-glance-join-but-null "\n  - "
                                                                          (list
                                                                           (when encrypted "Encrypted")
                                                                           (when linked "Contains links to third-party resources")
                                                                           (when repetitive (format "Repetitive%s"
                                                                                                    (if timestamps
                                                                                                        (format ", next active timestamp is " (car timestamps))
                                                                                                      "")))))))

                                      (when (and timestamps (not repetitive))
                                        (concat "- Schedule"
                                                (org-glance-join-but-null "\n  - " timestamps)))

                                      (if-let (projects (plist-get relations :projects))
                                          (concat "- Projects [/]"
                                                  (org-glance-join-but-null "\n  " (mapcar #'org-glance-relation-interpreter projects))))

                                      (if-let (subtasks (plist-get relations :subtasks))
                                          (concat "- Subtasks [/]"
                                                  (org-glance-join-but-null "\n  " (mapcar #'org-glance-relation-interpreter subtasks))))

                                      (if-let (mentions (plist-get relations :mentions))
                                          (concat "- Mentions"
                                                  (org-glance-join-but-null "\n  " (mapcar #'org-glance-relation-interpreter mentions))))))))
         (condition-case nil
             (org-update-checkbox-count-maybe)
           (error nil))
         (buffer-string))))))

(cl-defmacro org-glance-headline:with-headline-at-point (&rest forms)
  `(save-excursion
     (save-restriction
       (org-glance-headline:search-parents)
       (org-narrow-to-subtree)
       ,@forms)))

(org-glance:provide)
